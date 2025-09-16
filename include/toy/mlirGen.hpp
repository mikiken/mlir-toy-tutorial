#ifndef TOY_MLIRGEN_HPP
#define TOY_MLIRGEN_HPP

#include <memory>

namespace mlir {
class MLIRContext;
template <typename OpTy> class OwningOpRef;
class ModuleOp;
} // namespace mlir

namespace toy {
class ModuleAST;

/// Emit IR for the given Toy moduleAST, returns a newly created MLIR module
/// or nullptr on error.
mlir::OwningOpRef<mlir::ModuleOp> mlirGen(mlir::MLIRContext &context,
                                          const ModuleAST &moduleAST);
} // namespace toy

#endif // TOY_MLIRGEN_HPP