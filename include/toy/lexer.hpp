#ifndef TOY_LEXER_HPP
#define TOY_LEXER_HPP

#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <memory>

namespace toy {

/// Structure definition a location in a file.
struct Location {
  std::shared_ptr<std::string> file; ///< filename.
  int line;                          ///< line number.
  int column;                        ///< column number.
};

// List of Token returned by the lexer.
enum struct Token : int {
  Semicolon = ';',
  Parenthesis_open = '(',
  Parenthesis_close = ')',
  Brace_open = '{',
  Brace_close = '}',
  Bracket_open = '[',
  Bracket_close = ']',

  EOF_ = -1, // end of file

  // keywords
  Return = -2, // return
  Var = -3,    // var
  Def = -4,    // def

  // primary
  Identifier = -5, // identifier
  Number = -6,     // number
};

/// The Lexer is an abstract base class providing all the facilities that the
/// Parser expects. It goes through the stream one token at a time and keeps
/// track of the location in the file for debugging purposes.
/// It relies on a subclass to provide a `readNextLine()` method. The subclass
/// can proceed by reading the next line from the standard input or from a
/// memory mapped file.
class Lexer {
public:
  /// Create a lexer for the given filename. The filename is kept only for
  /// debugging purposes (attaching a location to a Token).
  Lexer(std::string filename)
      : lastLocation(
            {std::make_shared<std::string>(std::move(filename)), 0, 0}) {}
  virtual ~Lexer() = default;

  /// Look at the current token in the stream.
  Token getCurrentToken() { return currentToken; }

  /// Move to the next token in the stream and return it.
  Token getNextToken() { return currentToken = getToken(); }

  /// Move to the next token in the stream, asserting on the current token
  /// matching the expectation.
  void consume(Token token) {
    assert(token == currentToken && "consume Token mismatch expectation");
    getNextToken();
  }

  /// Return the current identifier
  /// (prereq: getCurrentToken() == Token::Identifier)
  llvm::StringRef getIdentifier() {
    assert(currentToken == Token::Identifier);
    return identifierStr;
  }

  /// Return the current number
  /// (prereq: getCurrentToken() == Token::Number)
  double getValue() {
    assert(currentToken == Token::Number);
    return numberValue;
  }

  /// Return the location for the beginning of the current token.
  Location getLastLocation() { return lastLocation; }

  // Return the current line in the file.
  int getLine() { return currentLine; }

  // Return the current column in the file.
  int getColumn() { return currentColumn; }

private:
  /// Delegate to a derived class fetching the next line. Returns an empty
  /// string to signal end of file (EOF). Lines are expected to always finish
  /// with "\n"
  virtual llvm::StringRef readNextLine() = 0;

  /// Return the next character from the stream. This manages the buffer for the
  /// current line and request the next line buffer to the derived class as
  /// needed.
  int getNextChar() {
    // The current line buffer should not be empty unless it is the end of file.
    if (currentLineBuffer.empty())
      return EOF;
    currentColumn++;
    auto nextchar = currentLineBuffer.front();
    currentLineBuffer = currentLineBuffer.drop_front();
    if (currentLineBuffer.empty())
      currentLineBuffer = readNextLine();
    if (nextchar == '\n') {
      currentLine++;
      currentColumn = 0;
    }
    return nextchar;
  }

  ///  Return the next token from standard input.
  Token getToken() {
    // Skip any whitespace.
    while (isspace(static_cast<int>(lastChar)))
      lastChar = Token(getNextChar());

    // Save the current location before reading the token characters.
    lastLocation.line = currentLine;
    lastLocation.column = currentColumn;

    // Identifier: [a-zA-Z][a-zA-Z0-9_]*
    if (isalpha(static_cast<int>(lastChar))) {
      identifierStr = static_cast<char>(lastChar);
      while (isalnum(static_cast<int>(lastChar = Token(getNextChar()))) ||
             lastChar == Token('_'))
        identifierStr += static_cast<char>(lastChar);

      if (identifierStr == "return")
        return Token::Return;
      if (identifierStr == "def")
        return Token::Def;
      if (identifierStr == "var")
        return Token::Var;

      return Token::Identifier;
    }

    // Number: [0-9.]+
    if (isdigit(static_cast<int>(lastChar)) || lastChar == Token('.')) {
      std::string numStr;
      do {
        numStr += static_cast<int>(lastChar);
        lastChar = Token(getNextChar());
      } while (isdigit(static_cast<int>(lastChar)) || lastChar == Token('.'));

      numberValue = strtod(numStr.c_str(), nullptr);
      return Token::Number;
    }

    if (lastChar == Token('#')) {
      // Comment until end of line.
      do {
        lastChar = Token(getNextChar());
      } while (lastChar != Token(EOF) && lastChar != Token('\n') &&
               lastChar != Token('\r'));

      if (lastChar != Token(EOF))
        return getToken();
    }

    // Check for end of file.  Don't eat the EOF.
    if (lastChar == Token(EOF))
      return Token::EOF_;

    // Otherwise, just return the character as its ascii value.
    Token thisChar = Token(lastChar);
    lastChar = Token(getNextChar());
    return thisChar;
  }

  /// The last token read from the input.
  Token currentToken = Token::EOF_;

  /// Location for `curTok`.
  Location lastLocation;

  /// If the current Token is an identifier, this string contains the value.
  std::string identifierStr;

  /// If the current Token is a number, this contains the value.
  double numberValue = 0;

  /// The last value returned by getNextChar(). We need to keep it around as we
  /// always need to read ahead one character to decide when to end a token and
  /// we can't put it back in the stream after reading from it.
  Token lastChar = Token(' ');

  /// Keep track of the current line number in the input stream
  int currentLine = 0;

  /// Keep track of the current column number in the input stream
  int currentColumn = 0;

  /// Buffer supplied by the derived class on calls to `readNextLine()`
  llvm::StringRef currentLineBuffer = "\n";
};

/// A lexer implementation operating on a buffer in memory.
class LexerBuffer final : public Lexer {
public:
  LexerBuffer(const char *begin, const char *end, std::string filename)
      : Lexer(std::move(filename)), current(begin), end(end) {}

private:
  /// Provide one line at a time to the Lexer, return an empty string when
  /// reaching the end of the buffer.
  llvm::StringRef readNextLine() override {
    auto *begin = current;
    while (current <= end && *current && *current != '\n')
      current++;
    if (current <= end && *current)
      current++;
    llvm::StringRef result{begin, static_cast<size_t>(current - begin)};
    return result;
  }
  const char *current, *end;
};

} // namespace toy

#endif // TOY_LEXER_HPP