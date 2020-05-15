cmake_minimum_required(VERSION 3.16.3)

project(SOMETHING LANGUAGES CXX CUDA)

set(CMAKE_CXX_STANDARD 17)

file(GLOB cuda_source_files
    "${SOMETHING_SOURCE_DIR}/src/*.cu"
    "${SOMETHING_SOURCE_DIR}/src/extern/*.cu"
)

include_directories(
    ${SOMETHING_SOURCE_DIR}/src
    ${SOMETHING_SOURCE_DIR}/src/extern
)

add_library(somethingLib SHARED ${cpp_source_files} ${cuda_source_files})

add_executable(something ${SOMETHING_SOURCE_DIR}/prog/main.cu)
target_compile_options(something PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:--ptxas-options=-v>) 

target_link_libraries( something
    somethingLib
)