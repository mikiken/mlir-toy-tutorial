#include "toy/mlirGen.hpp"
#include "toy/ast.hpp"
#include "toy/dialect.hpp"

#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/Support/raw_ostream.h"
#include <numeric>

namespace {

/// Implementation of a simple MLIR emission from the Toy AST.
///
/// This will emit operations that are specific to the Toy language, preserving
/// the semantics of the language and (hopefully) allow to perform accurate
/// analysis and transformation based on these high level semantics.
class MLIRGenImpl {
public:
  MLIRGenImpl(mlir::MLIRContext &context) : builder(&context) {}

  mlir::ModuleOp mlirGen(const toy::ModuleAST &moduleAST) {
    // We create an empty MLIR module and codegen functions one at a time and
    // add them to the module.
    theModule = mlir::ModuleOp::create(builder.getUnknownLoc());

    for (const auto &f : moduleAST) {
      if (!mlirGen(f))
        return nullptr;
    }

    if (failed(mlir::verify(theModule))) {
      theModule.emitError("module verification error");
      return nullptr;
    }

    return theModule;
  }

private:
  mlir::ModuleOp theModule;
  mlir::OpBuilder builder;
  llvm::ScopedHashTable<llvm::StringRef, mlir::Value> symbolTable;

  mlir::Location loc(const toy::Location &location) {
    return mlir::FileLineColLoc::get(builder.getStringAttr(*location.file),
                                     location.line, location.column);
  }

  mlir::LogicalResult declare(llvm::StringRef var, mlir::Value value) {
    if (symbolTable.count(var)) {
      return mlir::failure();
    }
    symbolTable.insert(var, value);
    return mlir::success();
  }

  mlir::toy::FuncOp mlirGen(const toy::PrototypeAST &proto) {
    auto location = loc(proto.getLocation());
    llvm::SmallVector<mlir::Type, 4> argTypes(proto.getArgs().size(),
                                              getType(toy::VarType{}));
    auto funcType = builder.getFunctionType(argTypes, {});
    return builder.create<mlir::toy::FuncOp>(location, proto.getName(),
                                             funcType);
  }

  mlir::toy::FuncOp mlirGen(const toy::FunctionAST &funcAST) {
    llvm::ScopedHashTableScope<llvm::StringRef, mlir::Value> varScope(
        symbolTable);
    builder.setInsertionPointToEnd(theModule.getBody());
    mlir::toy::FuncOp function = mlirGen(*funcAST.getProto());
    if (!function)
      return nullptr;

    mlir::Block &entryBlock = function.front();
    auto protoArgs = funcAST.getProto()->getArgs();
    for (const auto nameValue :
         llvm::zip(protoArgs, entryBlock.getArguments())) {
      if (failed(declare(std::get<0>(nameValue)->getName(),
                         std::get<1>(nameValue))))
        return nullptr;
    }

    builder.setInsertionPointToStart(&entryBlock);
    if (mlir::failed(mlirGen(*funcAST.getBody()))) {
      function.erase();
      return nullptr;
    }

    mlir::toy::ReturnOp returnOp;
    if (!entryBlock.empty())
      returnOp = llvm::dyn_cast<mlir::toy::ReturnOp>(entryBlock.back());
    if (!returnOp) {
      builder.create<mlir::toy::ReturnOp>(
          loc(funcAST.getProto()->getLocation()));
    } else if (returnOp.hasOperand()) {
      function.setType(builder.getFunctionType(
          function.getFunctionType().getInputs(), getType(toy::VarType{})));
    }

    return function;
  }

  mlir::Value mlirGen(const toy::ExprAST &expr) {
    switch (expr.getKind()) {
    case toy::ExprAST::Kind::BinOp:
      return mlirGen(llvm::cast<toy::BinaryExprAST>(expr));
    case toy::ExprAST::Kind::Var:
      return mlirGen(llvm::cast<toy::VariableExprAST>(expr));
    case toy::ExprAST::Kind::Literal:
      return mlirGen(llvm::cast<toy::LiteralExprAST>(expr));
    case toy::ExprAST::Kind::Call:
      return mlirGen(llvm::cast<toy::CallExprAST>(expr));
    case toy::ExprAST::Kind::Number:
      return mlirGen(llvm::cast<toy::NumberExprAST>(expr));
    default:
      mlir::emitError(loc(expr.getLocation()))
          << "MLIR codegen encountered an unhandled expr kind: "
          << (int)expr.getKind();
      return nullptr;
    }
  }

  mlir::Value mlirGen(const toy::BinaryExprAST &binop) {
    mlir::Value lhs = mlirGen(*binop.getLHS());
    if (!lhs)
      return nullptr;
    mlir::Value rhs = mlirGen(*binop.getRHS());
    if (!rhs)
      return nullptr;
    auto location = loc(binop.getLocation());

    switch (binop.getOp()) {
    case toy::Token('+'):
      return builder.create<mlir::toy::AddOp>(location, lhs, rhs);
    case toy::Token('*'):
      return builder.create<mlir::toy::MulOp>(location, lhs, rhs);
    }

    mlir::emitError(location)
        << "invalid binary operator '" << (char)binop.getOp() << "'";
    return nullptr;
  }

  mlir::Value mlirGen(const toy::VariableExprAST &expr) {
    if (auto variable = symbolTable.lookup(expr.getName()))
      return variable;

    mlir::emitError(loc(expr.getLocation()))
        << "error: unknown variable '" << expr.getName() << "'";
    return nullptr;
  }

  mlir::LogicalResult mlirGen(const toy::ReturnExprAST &ret) {
    auto location = loc(ret.getLocation());
    mlir::Value expr = nullptr;
    if (auto optionalExpr = ret.getExpr()) {
      if (!(expr = mlirGen(**optionalExpr)))
        return mlir::failure();
    }
    builder.create<mlir::toy::ReturnOp>(
        location, expr ? llvm::ArrayRef(expr) : llvm::ArrayRef<mlir::Value>());
    return mlir::success();
  }

  void collectData(const toy::ExprAST &expr, std::vector<double> &data) {
    if (const auto *lit = llvm::dyn_cast<toy::LiteralExprAST>(&expr)) {
      for (const auto &value : lit->getValues())
        collectData(*value, data);
      return;
    }

    assert(llvm::isa<toy::NumberExprAST>(expr) &&
           "expected literal or number expr");
    data.push_back(llvm::cast<toy::NumberExprAST>(expr).getValue());
  }

  mlir::Value mlirGen(const toy::LiteralExprAST &lit) {
    auto type = getType(lit.getDims());
    std::vector<double> data;
    data.reserve(std::accumulate(lit.getDims().begin(), lit.getDims().end(), 1,
                                 std::multiplies<int>()));
    collectData(lit, data);

    mlir::Type elementType = builder.getF64Type();
    auto dataType = mlir::RankedTensorType::get(lit.getDims(), elementType);
    auto dataAttribute =
        mlir::DenseElementsAttr::get(dataType, llvm::ArrayRef(data));
    return builder.create<mlir::toy::ConstantOp>(loc(lit.getLocation()), type,
                                                 dataAttribute);
  }

  mlir::Value mlirGen(const toy::CallExprAST &call) {
    llvm::StringRef callee = call.getCallee();
    auto location = loc(call.getLocation());
    llvm::SmallVector<mlir::Value, 4> operands;
    for (auto &expr : call.getArgs()) {
      auto arg = mlirGen(*expr);
      if (!arg)
        return nullptr;
      operands.push_back(arg);
    }

    if (callee == "transpose") {
      if (call.getArgs().size() != 1) {
        mlir::emitError(location)
            << "MLIR codegen encountered an error: toy.transpose "
               "does not accept multiple arguments";
        return nullptr;
      }
      return builder.create<mlir::toy::TransposeOp>(location, operands[0]);
    }

    return builder.create<mlir::toy::GenericCallOp>(location, callee, operands);
  }

  mlir::LogicalResult mlirGen(const toy::PrintExprAST &call) {
    auto arg = mlirGen(*call.getArg());
    if (!arg)
      return mlir::failure();
    builder.create<mlir::toy::PrintOp>(loc(call.getLocation()), arg);
    return mlir::success();
  }

  mlir::Value mlirGen(const toy::NumberExprAST &num) {
    return builder.create<mlir::toy::ConstantOp>(loc(num.getLocation()),
                                                 num.getValue());
  }

  mlir::Value mlirGen(const toy::VarDeclExprAST &vardecl) {
    const auto *init = vardecl.getInitValue();
    if (!init) {
      mlir::emitError(loc(vardecl.getLocation()))
          << "missing initializer in variable declaration";
      return nullptr;
    }

    mlir::Value value = mlirGen(*init);
    if (!value)
      return nullptr;

    if (!vardecl.getType().shape.empty()) {
      value = builder.create<mlir::toy::ReshapeOp>(
          loc(vardecl.getLocation()), getType(vardecl.getType()), value);
    }

    if (failed(declare(vardecl.getName(), value)))
      return nullptr;
    return value;
  }

  mlir::LogicalResult mlirGen(const toy::ExprASTList &blockAST) {
    llvm::ScopedHashTableScope<llvm::StringRef, mlir::Value> varScope(
        symbolTable);
    for (const auto &expr : blockAST) {
      if (const auto *vardecl =
              llvm::dyn_cast<toy::VarDeclExprAST>(expr.get())) {
        if (!mlirGen(*vardecl))
          return mlir::failure();
        continue;
      }
      if (const auto *ret = llvm::dyn_cast<toy::ReturnExprAST>(expr.get()))
        return mlirGen(*ret);
      if (const auto *print = llvm::dyn_cast<toy::PrintExprAST>(expr.get())) {
        if (mlir::failed(mlirGen(*print)))
          return mlir::failure();
        continue;
      }

      if (!mlirGen(*expr))
        return mlir::failure();
    }
    return mlir::success();
  }

  mlir::Type getType(llvm::ArrayRef<int64_t> shape) {
    if (shape.empty())
      return mlir::UnrankedTensorType::get(builder.getF64Type());
    return mlir::RankedTensorType::get(shape, builder.getF64Type());
  }

  mlir::Type getType(const toy::VarType &type) { return getType(type.shape); }
};

} // namespace

namespace toy {

mlir::OwningOpRef<mlir::ModuleOp> mlirGen(mlir::MLIRContext &context,
                                          const ModuleAST &moduleAST) {
  return MLIRGenImpl(context).mlirGen(moduleAST);
}

} // namespace toy
