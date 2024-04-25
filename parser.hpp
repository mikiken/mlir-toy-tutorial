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

  std::unique_ptr<ExprASTList> parseBlock() {}
  std::unique_ptr<PrototypeAST> parsePrototype() {}
  std::unique_ptr<FunctionAST> parseDefinition() {}

  template <typename R, typename T, typename U = const char *>
  std::unique_ptr<R> parseError(T &&expected, U &&context = "") {}
};

} // namespace toy

#endif // TOY_PARSER_HPP