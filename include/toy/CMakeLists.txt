add_mlir_dialect(ToyOps toy)

set(LLVM_TARGET_DEFINITIONS ops.td)
mlir_tablegen(dialect.hpp.inc -gen-dialect-decls)
mlir_tablegen(dialect.cpp.inc -gen-dialect-defs)
mlir_tablegen(ops.hpp.inc -gen-op-decls)
mlir_tablegen(ops.cpp.inc -gen-op-defs)
add_public_tablegen_target(ToyOpsIncGen)