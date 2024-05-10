cmake_minimum_required(VERSION 3.20)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_program(LLVM_CONFIG_EXECUTABLE NAMES llvm-config-16 llvm-config)
if(LLVM_CONFIG_EXECUTABLE)
    execute_process(COMMAND ${LLVM_CONFIG_EXECUTABLE} --cmakedir OUTPUT_VARIABLE LLVM_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
    message(STATUS "Set LLVM_DIR: ${LLVM_DIR}")
else()
    message(FATAL_ERROR "llvm-config-16 not found")
endif()

find_program(LLVM_CONFIG_EXECUTABLE NAMES llvm-config-16 llvm-config)
if(LLVM_CONFIG_EXECUTABLE)
    execute_process(COMMAND ${LLVM_CONFIG_EXECUTABLE} --prefix OUTPUT_VARIABLE MLIR_DIR_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(MLIR_DIR "${MLIR_DIR_PREFIX}/lib/cmake/mlir")
    message(STATUS "Set MLIR_DIR: ${MLIR_DIR}")
else()
    message(FATAL_ERROR "llvm-config-16 not found")
endif()

project(mlir-toy-tutorial CXX C)

find_package(LLVM REQUIRED CONFIG)
find_package(MLIR REQUIRED CONFIG)

add_subdirectory(src)
