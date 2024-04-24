#ifndef TOY_AST_HPP
#define TOY_AST_HPP

#include "lexer.hpp"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include <optional>
#include <utility>
#include <vector>

namespace toy {

/// A variable type with shape information.
struct VarType {
  std::vector<int64_t> shape;
};

/// Base class for all expression nodes.
class ExprAST {
public:
  enum struct Kind {
    VarDecl,
    Return,
    Number,
    Literal,
    Var,
    BinOp,
    Call,
    Print,
  };

  ExprAST(ExprAST::Kind kind, Location location)
      : kind(kind), location(std::move(location)) {}
  virtual ~ExprAST() = default;

  ExprAST::Kind getKind() const { return kind; }

  const Location &getLocation() { return location; }

private:
  const ExprAST::Kind kind;
  Location location;
};

/// A block list of expressions.
// equivalent to `typedef std::vector<std::unique_ptr<ExprAST>> ExprASTList`;
using ExprASTList = std::vector<std::unique_ptr<ExprAST>>;

/// Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double value;

public:
  NumberExprAST(Location location, double value)
      : ExprAST(ExprAST::Kind::Number, std::move(location)), value(value) {}

  double getValue() { return value; }

  /// LLVM style RTTI
  // Run-Time Type Identification (実行時型情報)
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Number;
  }
};

/// Expression class for a literal value.
class LiteralExprAST : public ExprAST {
  ExprASTList values;
  std::vector<int64_t> dims;

public:
  LiteralExprAST(Location location, ExprASTList values,
                 std::vector<int64_t> dims)
      : ExprAST(ExprAST::Kind::Literal, std::move(location)),
        values(std::move(values)), dims(std::move(dims)) {}

  llvm::ArrayRef<std::unique_ptr<ExprAST>> getValues() { return values; }
  llvm::ArrayRef<int64_t> getDims() { return dims; }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Literal;
  }
};

/// Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string name;

public:
  VariableExprAST(Location location, llvm::StringRef name)
      : ExprAST(ExprAST::Kind::Var, std::move(location)), name(name) {}

  llvm::StringRef getName() { return name; }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Var;
  }
};

/// Expression class for defining a variable.
class VarDeclExprAST : public ExprAST {
  std::string name;
  VarType type;
  std::unique_ptr<ExprAST> initValue;

public:
  VarDeclExprAST(Location location, llvm::StringRef name, VarType type,
                 std::unique_ptr<ExprAST> initValue)
      : ExprAST(ExprAST::Kind::VarDecl, std::move(location)), name(name),
        type(std::move(type)), initValue(std::move(initValue)) {}

  llvm::StringRef getName() { return name; }
  ExprAST *getInitValue() { return initValue.get(); }
  const VarType &getType() { return type; }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::VarDecl;
  }
};

/// Expression class for a return operator.
class ReturnExprAST : public ExprAST {
  std::optional<std::unique_ptr<ExprAST>> expr;

public:
  ReturnExprAST(Location location, std::optional<std::unique_ptr<ExprAST>> expr)
      : ExprAST(ExprAST::Kind::Return, std::move(location)),
        expr(std::move(expr)) {}

  std::optional<ExprAST *> getExpr() {
    if (expr.has_value())
      return expr->get();
    return std::nullopt;
  }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Return;
  }
};

/// Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char op;
  std::unique_ptr<ExprAST> lhs, rhs;

public:
  char getOp() { return op; }
  ExprAST *getLHS() { return lhs.get(); }
  ExprAST *getRHS() { return rhs.get(); }

  BinaryExprAST(Location location, char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : ExprAST(ExprAST::Kind::BinOp, std::move(location)), op(op),
        lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::BinOp;
  }
};

/// Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string callee;
  ExprASTList args;

public:
  CallExprAST(Location location, const std::string &callee, ExprASTList args)
      : ExprAST(ExprAST::Kind::Call, std::move(location)), callee(callee),
        args(std::move(args)) {}

  llvm::StringRef getCallee() { return callee; }
  llvm::ArrayRef<std::unique_ptr<ExprAST>> getArgs() { return args; }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Call;
  }
};

/// Expression class for builtin print calls.
class PrintExprAST : public ExprAST {
  std::unique_ptr<ExprAST> arg;

public:
  PrintExprAST(Location location, std::unique_ptr<ExprAST> arg)
      : ExprAST(ExprAST::Kind::Print, std::move(location)),
        arg(std::move(arg)) {}

  ExprAST *getArg() { return arg.get(); }

  /// LLVM style RTTI
  static bool classof(const ExprAST *c) {
    return c->getKind() == ExprAST::Kind::Print;
  }
};

/// This class represents the "prototype" for a function, which captures its
/// name, and its argument names (thus implicitly the number of arguments the
/// function takes).
class PrototypeAST {
  Location location;
  std::string name;
  std::vector<std::unique_ptr<VariableExprAST>> args;

public:
  PrototypeAST(Location location, const std::string &name,
               std::vector<std::unique_ptr<VariableExprAST>> args)
      : location(std::move(location)), name(name), args(std::move(args)) {}

  const Location &getLocation() { return location; }
  llvm::StringRef getName() const { return name; }
  llvm::ArrayRef<std::unique_ptr<VariableExprAST>> getArgs() { return args; }
};

/// This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> proto;
  std::unique_ptr<ExprASTList> body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprASTList> body)
      : proto(std::move(proto)), body(std::move(body)) {}
  PrototypeAST *getProto() { return proto.get(); }
  ExprASTList *getBody() { return body.get(); }
};

/// This class represents a list of functions to be processed together.
class ModuleAST {
  std::vector<FunctionAST> functions;

public:
  ModuleAST(std::vector<FunctionAST> functions)
      : functions(std::move(functions)) {}

  auto begin() { return functions.begin(); }
  auto end() { return functions.end(); }
};

void dump(ModuleAST &);

} // namespace toy

#endif // TOY_AST_HPP