#include "toy/ast.hpp"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace toy;

namespace {

// RAII helper to manage increasing/decreasing the indentation as we traverse
// the AST
struct Indent {
  Indent(int &level) : level(level) { ++level; }
  ~Indent() { --level; }
  int &level;
};

/// Helper class that implement the AST tree traversal and print the nodes along
/// the way. The only data member is the current indentation level.
class ASTDumper {
public:
  void dump(ModuleAST *node);

private:
  void dump(const VarType &type);
  void dump(VarDeclExprAST *varDecl);
  void dump(ExprAST *expr);
  void dump(ExprASTList *exprList);
  void dump(NumberExprAST *num);
  void dump(LiteralExprAST *node);
  void dump(VariableExprAST *node);
  void dump(ReturnExprAST *node);
  void dump(BinaryExprAST *node);
  void dump(CallExprAST *node);
  void dump(PrintExprAST *node);
  void dump(PrototypeAST *node);
  void dump(FunctionAST *node);

  // Actually print spaces matching the current indentation level
  void indent() {
    for (int i = 0; i < currentIndent; i++)
      llvm::errs() << "  ";
  }
  int currentIndent = 0;
};

} // namespace

/// Return a formatted string for the location of any node
template <typename T> static std::string location(T *node) {
  const auto &location = node->getLocation();
  return (llvm::Twine("@") + *location.file + ":" + llvm::Twine(location.line) +
          ":" + llvm::Twine(location.column))
      .str();
}

// Helper Macro to bump the indentation level and print the leading spaces for
// the current indentations
#define INDENT()                                                               \
  Indent level_(currentIndent);                                                \
  indent();

/// Dispatch to a generic expressions to the appropriate subclass using RTTI
void ASTDumper::dump(ExprAST *expr) {
  llvm::TypeSwitch<ExprAST *>(expr)
      .Case<BinaryExprAST, CallExprAST, LiteralExprAST, NumberExprAST,
            PrintExprAST, ReturnExprAST, VarDeclExprAST, VariableExprAST>(
          [&](auto *node) { this->dump(node); })
      .Default([&](ExprAST *) {
        // No match, fallback to a generic message
        INDENT();
        llvm::errs() << "<unknown Expr, kind "
                     << static_cast<char>(expr->getKind()) << ">\n";
      });
}

/// A variable declaration is printing the variable name, the type, and then
/// recurse in the initializer value.
void ASTDumper::dump(VarDeclExprAST *varDecl) {
  INDENT();
  llvm::errs() << "VarDecl " << varDecl->getName();
  dump(varDecl->getType());
  llvm::errs() << " " << location(varDecl) << "\n";
  dump(varDecl->getInitValue());
}

/// A "block", or a list of expression
void ASTDumper::dump(ExprASTList *exprList) {
  INDENT();
  llvm::errs() << "Block {\n";
  for (auto &expr : *exprList)
    dump(expr.get());
  indent();
  llvm::errs() << "} // Block\n";
}

/// A literal number, just print the value.
void ASTDumper::dump(NumberExprAST *num) {
  INDENT();
  llvm::errs() << num->getValue() << " " << location(num) << "\n";
}

/// Helper to print recursively a literal. This handles nested array like:
///    [ [ 1, 2 ], [ 3, 4 ] ]
/// We print out such array with the dimensions spelled out at every level:
///    <2,2>[<2>[ 1, 2 ], <2>[ 3, 4 ] ]
void printLiteralHelper(ExprAST *litOrNum) {
  // Inside a literal expression we can have either a number or another literal
  if (auto *num = llvm::dyn_cast<NumberExprAST>(litOrNum)) {
    llvm::errs() << num->getValue();
    return;
  }
  auto *literal = llvm::cast<LiteralExprAST>(litOrNum);

  // Print the dimension for this literal first
  llvm::errs() << "<";
  llvm::interleaveComma(literal->getDims(), llvm::errs());
  llvm::errs() << ">";

  // Now print the content, recursing on every element of the list
  llvm::errs() << "[ ";
  llvm::interleaveComma(literal->getValues(), llvm::errs(),
                        [&](auto &elt) { printLiteralHelper(elt.get()); });
  llvm::errs() << "]";
}

/// Print a literal, see the recursive helper above for the implementation.
void ASTDumper::dump(LiteralExprAST *node) {
  INDENT();
  llvm::errs() << "Literal: ";
  printLiteralHelper(node);
  llvm::errs() << " " << location(node) << "\n";
}

/// Print a variable reference (just a name).
void ASTDumper::dump(VariableExprAST *node) {
  INDENT();
  llvm::errs() << "var: " << node->getName() << " " << location(node) << "\n";
}

/// Return statement print the return and its (optional) argument.
void ASTDumper::dump(ReturnExprAST *node) {
  INDENT();
  llvm::errs() << "Return\n";
  if (node->getExpr().has_value())
    return dump(*node->getExpr());
  {
    INDENT();
    llvm::errs() << "(void)\n";
  }
}

/// Print a binary operation, first the operator, then recurse into LHS and RHS.
void ASTDumper::dump(BinaryExprAST *node) {
  INDENT();
  llvm::errs() << "BinOp: " << static_cast<char>(node->getOp()) << " "
               << location(node) << "\n";
  dump(node->getLHS());
  dump(node->getRHS());
}

/// Print a call expression, first the callee name and the list of args by
/// recursing into each individual argument.
void ASTDumper::dump(CallExprAST *node) {
  INDENT();
  llvm::errs() << "Call '" << node->getCallee() << "' [ " << location(node)
               << "\n";
  for (auto &arg : node->getArgs())
    dump(arg.get());
  indent();
  llvm::errs() << "]\n";
}

/// Print a builtin print call, first the builtin name and then the argument.
void ASTDumper::dump(PrintExprAST *node) {
  INDENT();
  llvm::errs() << "Print [ " << location(node) << "\n";
  dump(node->getArg());
  indent();
  llvm::errs() << "]\n";
}

/// Print type: only the shape is printed in between '<' and '>'
void ASTDumper::dump(const VarType &type) {
  llvm::errs() << "<";
  llvm::interleaveComma(type.shape, llvm::errs());
  llvm::errs() << ">";
}

/// Print a function prototype, first the function name, and then the list of
/// parameters names.
void ASTDumper::dump(PrototypeAST *node) {
  INDENT();
  llvm::errs() << "Proto '" << node->getName() << "' " << location(node)
               << "\n";
  indent();
  llvm::errs() << "Params: [";
  llvm::interleaveComma(node->getArgs(), llvm::errs(),
                        [](auto &arg) { llvm::errs() << arg->getName(); });
  llvm::errs() << "]\n";
}

/// Print a function, first the prototype and then the body.
void ASTDumper::dump(FunctionAST *node) {
  INDENT();
  llvm::errs() << "Function \n";
  dump(node->getProto());
  dump(node->getBody());
}

/// Print a module, actually loop over the functions and print them in sequence.
void ASTDumper::dump(ModuleAST *node) {
  INDENT();
  llvm::errs() << "Module:\n";
  for (auto &f : *node)
    dump(&f);
}

namespace toy {

// Public API
void dump(ModuleAST &module) { ASTDumper().dump(&module); }

} // namespace toy