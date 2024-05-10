cmake_minimum_required(VERSION 3.20)

add_subdirectory(parser)

add_executable(toyc toyc.cpp)
target_link_libraries(toyc parser)
target_include_directories(toyc PUBLIC ${PROJECT_SOURCE_DIR}/include)

include_directories(${LLVM_INCLUDE_DIRS})
target_link_directories(toyc PRIVATE ${LLVM_LIBRARY_DIRS})
target_link_libraries(toyc LLVM-16)