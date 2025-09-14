add_library(mlir STATIC dialect.cpp mlirGen.cpp)
target_include_directories(mlir PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)
# .inc ファイルを生成するターゲット (ToyOpsIncGen) への依存関係を追加
add_dependencies(mlir ToyOpsIncGen)

include_directories(${LLVM_INCLUDE_DIRS})
target_link_directories(mlir PRIVATE ${LLVM_LIBRARY_DIRS})
target_link_libraries(mlir LLVM-16)

include_directories(${MLIR_INCLUDE_DIRS})
target_link_directories(mlir PRIVATE ${MLIR_LIBRARY_DIRS} "${CMAKE_BINARY_DIR}/include")
target_link_libraries(mlir MLIR)
