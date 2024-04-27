#ifndef TOY_PARSER_HPP
#define TOY_PARSER_HPP

#include "ast.hpp"
#include "lexer.hpp"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <optional>
#include <utility>
#include <vector>

namespace toy {

/// This is a simple recursive parser for the Toy language. It produces a well
/// formed AST from a stream of Token supplied by the Lexer. No semantic checks
/// or symbol resolution is performed. For example, variables are referenced by
/// string and the code could reference an undeclared variable and the parsing
/// succeeds.
class Parser {
public:
  /// Create a Parser for supplied lexer.
  Parser(Lexer &lexer) : lexer(lexer){};

  /// Parse a full Module. A module is a list of function definitions.
  std::unique_ptr<ModuleAST> parseModule() {
    lexer.getNextToken();

    // Parse functions one at a time and accumulate in this vector.
    std::vector<FunctionAST> functions;
    while (auto f = parseDefinition()) {
      functions.push_back(std::move(*f));
      if (lexer.getCurrentToken() == Token::EOF_)
        break;
    }
    // If we didn't reach EOF, there was an error during parsing.
    if (lexer.getCurrentToken() != Token::EOF_)
      return parseError<ModuleAST>("nothing", "at end of module");

    return std::make_unique<ModuleAST>(std::move(functions));
  }

private:
  Lexer &lexer;

  std::unique_ptr<ReturnExprAST> parseReturn() {}
  std::unique_ptr<ExprAST> parseNumberExpr() {}
  std::unique_ptr<ExprAST> parseTensorLiteralExpr() {}

  /// parenexpr ::= '(' expression ')'
  std::unique_ptr<ExprAST> parseParenExpr() {
    lexer.getNextToken(); // eat (
    auto v = parseExpression();
    if (!v)
      return nullptr;

    if (lexer.getCurrentToken() != Token(')'))
      return parseError<ExprAST>(")", "to close expression with parentheses");
    lexer.consume(Token(')'));
    return v;
  }

  /// identifierexpr
  ///   ::= identifier
  ///   ::= identifier '(' expression* ')'
  std::unique_ptr<ExprAST> parseIdentifierExpr() {
    std::string name(lexer.getIdentifier());

    auto location = lexer.getLastLocation();
    lexer.getNextToken(); // eat identifier

    if (lexer.getCurrentToken() != Token('(')) // Simple variable ref.
      return std::make_unique<VariableExprAST>(std::move(location), name);

    // Otherwise, this is a function call.
    lexer.consume(Token('('));
    ExprASTList args;
    if (lexer.getCurrentToken() != Token(')')) {
      while (true) {
        if (auto arg = parseExpression())
          args.push_back(std::move(arg));
        else
          return nullptr;

        if (lexer.getCurrentToken() == Token(')'))
          break;

        if (lexer.getCurrentToken() != Token(','))
          return parseError<ExprAST>(", or )", "in argument list");
        lexer.getNextToken();
      }
    }
    lexer.consume(Token(')'));

    // it can be a builtin call to print
    if (name == "print") {
      if (args.size() != 1)
        return parseError<ExprAST>("<single arg>",
                                   "as argument list to print()");

      return std::make_unique<PrintExprAST>(std::move(location),
                                            std::move(args[0]));
    }

    // Call to user defined function
    return std::make_unique<CallExprAST>(std::move(location), name,
                                         std::move(args));
  }

  /// primary
  ///   ::= identifierexpr
  ///   ::= numberexpr
  ///   ::= parenexpr
  ///   ::= tensorliteral
  std::unique_ptr<ExprAST> parsePrimary() {
    switch (lexer.getCurrentToken()) {
    case Token::Identifier:
      return parseIdentifierExpr();
    case Token::Number:
      return parseNumberExpr();
    case Token::Parenthesis_open:
      return parseParenExpr();
    case Token::Bracket_open:
      return parseTensorLiteralExpr();
    case Token(';'):
      return nullptr;
    case Token('}'):
      return nullptr;
    default:
      llvm::errs() << "unknown token '" << (char)lexer.getCurrentToken()
                   << "' when expecting an expression\n";
      return nullptr;
    }
  }

  /// Recursively parse the right hand side of a binary expression,
  /// the ExprPrecedence argument indicates the precedence of the current
  /// binary operator.
  ///
  /// binoprhs ::= ('+' primary)*
  std::unique_ptr<ExprAST> parseBinOpRHS(int exprPrecedence,
                                         std::unique_ptr<ExprAST> lhs) {
    // If this is a binop, find its precedence.
    while (true) {
      int tokenPrecedence = getTokenPrecedence();

      // If this is a binop that binds at least as tightly as the current binop,
      // consume it, otherwise we are done.
      if (tokenPrecedence < exprPrecedence)
        return lhs;

      // Okay, we know this is a binop.
      Token binOp = lexer.getCurrentToken();
      lexer.consume(binOp);
      auto location = lexer.getLastLocation();

      // Parse the primary expression after the binary operator.
      auto rhs = parsePrimary();
      if (!rhs)
        return parseError<ExprAST>("expression", "to complete binary operator");

      // If BinOp binds less tightly with rhs than the operator after rhs,
      // let the pending operator take rhs as its lhs.
      int nextPrecedence = getTokenPrecedence();
      if (tokenPrecedence < nextPrecedence) {
        rhs = parseBinOpRHS(tokenPrecedence + 1, std::move(rhs));
        if (!rhs)
          return nullptr;
      }

      // Merge LHS/RHS.
      lhs = std::make_unique<BinaryExprAST>(std::move(location), binOp,
                                            std::move(lhs), std::move(rhs));
    }
  }

  /// expression ::= primary binop rhs
  std::unique_ptr<ExprAST> parseExpression() {
    auto lhs = parsePrimary();
    if (!lhs)
      return nullptr;

    return parseBinOpRHS(0, std::move(lhs));
  }

  /// type ::= < shape_list >
  /// shape_list ::= num | num , shape_list
  std::unique_ptr<VarType> parseType() {
    if (lexer.getCurrentToken() != Token('<'))
      return parseError<VarType>("<", "to begin type");
    lexer.getNextToken(); // eat <

    auto type = std::make_unique<VarType>();

    while (lexer.getCurrentToken() == Token::Number) {
      type->shape.push_back(lexer.getValue());
      lexer.getNextToken();
      if (lexer.getCurrentToken() == Token(','))
        lexer.getNextToken();
    }

    if (lexer.getCurrentToken() != Token('>'))
      return parseError<VarType>(">", "to end type");
    lexer.getNextToken(); // eat >
    return type;
  }

  /// Parse a variable declaration, it starts with a `var` keyword followed by
  /// and identifier and an optional type (shape specification) before the
  /// initializer.
  /// decl ::= var identifier [ type ] = expr
  std::unique_ptr<VarDeclExprAST> parseDeclaration() {
    if (lexer.getCurrentToken() != Token::Var)
      return parseError<VarDeclExprAST>("var", "to begin declaration");
    auto location = lexer.getLastLocation();
    lexer.getNextToken(); // eat var

    if (lexer.getCurrentToken() != Token::Identifier)
      return parseError<VarDeclExprAST>("identifier",
                                        "after `var` declaration");
    std::string identifier(lexer.getIdentifier());
    lexer.getNextToken(); // eat identifier

    std::unique_ptr<VarType> type; // Type is optional, it can be inferred.
    if (lexer.getCurrentToken() == Token('<')) {
      type = parseType();
      if (!type)
        return nullptr;
    }

    if (!type)
      type = std::make_unique<VarType>();
    lexer.consume(Token('='));
    auto expr = parseExpression();
    return std::make_unique<VarDeclExprAST>(std::move(location),
                                            std::move(identifier),
                                            std::move(*type), std::move(expr));
  }

  /// Parse a block: a list of expression separated by semicolons and wrapped in
  /// curly braces.
  ///
  /// block ::= { expression_list }
  /// expression_list ::= block_expr ; expression_list
  /// block_expr ::= decl | "return" | expr
  std::unique_ptr<ExprASTList> parseBlock() {
    if (lexer.getCurrentToken() != Token('{'))
      return parseError<ExprASTList>("{", "to begin block");
    lexer.consume(Token('{'));

    auto exprList = std::make_unique<ExprASTList>();

    // Ignore empty expressions: swallow sequences of semicolons.
    while (lexer.getCurrentToken() == Token(';'))
      lexer.consume(Token(';'));

    while (lexer.getCurrentToken() != Token('}') &&
           lexer.getCurrentToken() != Token::EOF_) {
      if (lexer.getCurrentToken() == Token::Var) {
        // Variable declaration
        auto varDecl = parseDeclaration();
        if (!varDecl)
          return nullptr;
        exprList->push_back(std::move(varDecl));
      } else if (lexer.getCurrentToken() == Token::Return) {
        // Return statement
        auto ret = parseReturn();
        if (!ret)
          return nullptr;
        exprList->push_back(std::move(ret));
      } else {
        // General expression
        auto expr = parseExpression();
        if (!expr)
          return nullptr;
        exprList->push_back(std::move(expr));
      }
      // Ensure that elements are separated by a semicolon.
      if (lexer.getCurrentToken() != Token(';'))
        return parseError<ExprASTList>(";", "after expression");

      // Ignore empty expressions: swallow sequences of semicolons.
      while (lexer.getCurrentToken() == Token(';'))
        lexer.consume(Token(';'));
    }

    if (lexer.getCurrentToken() != Token('}'))
      parseError<ExprASTList>("}", "to close block");

    lexer.consume(Token('}'));
    return exprList;
  }

  /// prototype ::= def identifier '() decl_list ')'
  /// decl_list ::= identifier | identifier, decl_list
  std::unique_ptr<PrototypeAST> parsePrototype() {
    auto location = lexer.getLastLocation();

    if (lexer.getCurrentToken() != Token::Def)
      return parseError<PrototypeAST>("def", "in prototype");
    lexer.consume(Token::Def);

    if (lexer.getCurrentToken() != Token::Identifier)
      return parseError<PrototypeAST>("function name", "in prototype");

    std::string functionName(lexer.getIdentifier());
    lexer.consume(Token::Identifier);

    if (lexer.getCurrentToken() != Token('('))
      return parseError<PrototypeAST>("(", "in prototype");
    lexer.consume(Token('('));

    std::vector<std::unique_ptr<VariableExprAST>> args;
    if (lexer.getCurrentToken() != Token(')')) {
      do {
        std::string paramName(lexer.getIdentifier());
        auto location = lexer.getLastLocation();
        lexer.consume(Token::Identifier);
        auto decl =
            std::make_unique<VariableExprAST>(std::move(location), paramName);
        args.push_back(std::move(decl));
        if (lexer.getCurrentToken() != Token(','))
          break;
        lexer.consume(Token(','));
        if (lexer.getCurrentToken() != Token::Identifier)
          return parseError<PrototypeAST>("identifier",
                                          "in function parameter list");
      } while (true);
    }
    if (lexer.getCurrentToken() != Token(')'))
      return parseError<PrototypeAST>(")", "to end function prototype");

    // success
    lexer.consume(Token(')'));
    return std::make_unique<PrototypeAST>(std::move(location), functionName,
                                          std::move(args));
  }

  /// Parse a function definition, we expect a prototype initiated by with the
  /// `def` keyword, followed by a block containing a list of expressions.
  ///
  /// definition ::= prototype block
  std::unique_ptr<FunctionAST> parseDefinition() {
    auto proto = parsePrototype();
    if (!proto)
      return nullptr;

    if (auto block = parseBlock())
      return std::make_unique<FunctionAST>(std::move(proto), std::move(block));
    return nullptr;
  }

  /// Get the precedence of the pending binary operator token.
  int getTokenPrecedence() {
    if (!isascii((int)lexer.getCurrentToken()))
      return -1;

    // 1 is lowerest precedence.
    switch (static_cast<char>(lexer.getCurrentToken())) {
    case '-':
      return 20;
    case '+':
      return 20;
    case '*':
      return 40;
    default:
      return -1;
    }
  }

  /// Helper function to signal errors while parsing, it takes an argument
  /// indicating the expected token and another argument giving more context.
  /// Location is retrieved from the lexer to enrich the error message.
  template <typename R, typename T, typename U = const char *>
  std::unique_ptr<R> parseError(T &&expected, U &&context = "") {
    auto currentToken = lexer.getCurrentToken();
    llvm::errs() << "Parse error (" << lexer.getLastLocation().line << ", "
                 << lexer.getLastLocation().column << "): expected '"
                 << expected << "' " << context << " but has Token "
                 << currentToken;
    if (isprint(curToken))
      llvm::errs() << " '" << (char)curToken << "'";
    llvm::errs() << "\n";
    return nullptr;
  }
};

} // namespace toy

#endif // TOY_PARSER_HPP