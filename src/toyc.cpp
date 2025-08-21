#include "toy/ast.hpp"
#include "toy/dialect.hpp"
#include "toy/lexer.hpp"
#include "toy/parser.hpp"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "mlir/IR/MLIRContext.h"

#include <memory>
#include <string>
#include <system_error>

using namespace toy;
namespace cl = llvm::cl;

static cl::opt<std::string> inputFilename(cl::Positional,
                                          cl::desc("<input toy file>"),
                                          cl::init("-"),
                                          cl::value_desc("filename"));

enum InputType { Toy, MLIR };
static cl::opt<InputType> inputType();

enum class Action { None, DumpAST, DumpMLIR };

static cl::opt<Action> emitAction(
    "emit", cl::desc("Select the kind of output desired"),
    cl::values(clEnumValN(Action::DumpAST, "ast", "output the AST dump")));

// Returns a Toy AST resulting from parsing the file or a nullptr on error.
std::unique_ptr<toy::ModuleAST> parseInputFile(llvm::StringRef filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrError =
      llvm::MemoryBuffer::getFileOrSTDIN(filename);
  if (std::error_code errorCode = fileOrError.getError()) {
    llvm::errs() << "Could not open input file: " << errorCode.message()
                 << "\n";
    return nullptr;
  }
  auto buffer = fileOrError.get()->getBuffer();
  LexerBuffer lexer(buffer.begin(), buffer.end(), std::string(filename));
  Parser parser(lexer);
  return parser.parseModule();
}

int dumpMLIR() {}

// TODO: wrap dump() in this function
int dumpAST() {}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv, "toy compiler\n");

  auto moduleAST = parseInputFile(inputFilename);
  if (!moduleAST)
    return 1;

  switch (emitAction) {
  case Action::DumpAST:
    dump(*moduleAST);
  case Action::DumpMLIR:
    dumpMLIR();
  default:
    llvm::errs() << "No action specified (parsing only?), use -emit=<action>\n";
  }

  return 0;
}
