#ifndef TOY_DIALECT_HPP
#define TOY_DIALECT_HPP

#include "mlir/Bytecode/BytecodeImplementation.h"
#include "mlir/IR/Dialect.h"
#include "mlir/IR/FunctionInterfaces.h"
#include "mlir/IR/SymbolTable.h"
#include "mlir/Interfaces/CallInterfaces.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"

/// Include the auto-generated header containing the declaration of the toy
/// dialect.
#include "toy/dialect.hpp.inc"

/// Include the auto-generated header file containing the declarations of the
/// toy operations.
#define GET_OP_CLASSES
#include "toy/ops.hpp.inc"

#endif // TOY_DIALECT_HPP