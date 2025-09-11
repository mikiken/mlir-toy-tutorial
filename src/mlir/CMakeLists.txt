add_library(mlir STATIC dialect.cpp mlirGen.cpp)
target_include_directories(mlir PUBLIC ${PROJECT_SOURCE_DIR}/include)

include_directories(${LLVM_INCLUDE_DIRS})
target_link_directories(mlir PRIVATE ${LLVM_LIBRARY_DIRS})
target_link_libraries(mlir LLVM-16)

include_directories(${MLIR_INCLUDE_DIRS})
target_link_directories(mlir PRIVATE ${MLIR_LIBRARY_DIRS})
target_link_libraries(mlir MLIR)
