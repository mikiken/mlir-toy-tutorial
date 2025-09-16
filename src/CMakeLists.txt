cmake_minimum_required(VERSION 3.20)

add_subdirectory(parser)
add_subdirectory(mlir)

add_executable(toyc toyc.cpp)

# Add include directories
target_include_directories(toyc PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

# Link against our libraries
target_link_libraries(toyc PRIVATE parser mlir)

# Link against LLVM/MLIR
find_package(LLVM REQUIRED CONFIG)
find_package(MLIR REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Found MLIR ${MLIR_PACKAGE_VERSION}")

# MLIR and LLVM libraries are not modular, so we can just link against
# a common set of libraries.
# target_link_libraries(toyc PRIVATE ${MLIR_LIBS} ${LLVM_LIBS})
