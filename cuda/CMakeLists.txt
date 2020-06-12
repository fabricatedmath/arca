cmake_minimum_required(VERSION 3.17.3)

#set(CMAKE_VERBOSE_MAKEFILE ON)

project(SOMETHING LANGUAGES CXX CUDA)
#SET(CUDA_SEPARABLE_COMPILATION ON)
#find_package(CUDA REQUIRED)
#enable_language(CUDA)
set(CMAKE_CXX_STANDARD 17)
#set(CUDA_SEPARABLE_COMPILATION ON) 
#set(CMAKE_POSITION_INDEPENDENT_CODE ON)
#find_package(CUDA REQUIRED)

file(GLOB cuda_source_files
    "${SOMETHING_SOURCE_DIR}/src/*.cu"
    #"${SOMETHING_SOURCE_DIR}/src/extern/*.cu"
)

include_directories(
    ${SOMETHING_SOURCE_DIR}/src
    #${SOMETHING_SOURCE_DIR}/src/extern
)

#add_library(somethingPTX OBJECT ${cuda_source_files})
#target_compile_options(somethingPTX PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:--ptxas-options=-v --ptx -rdc=true -gencode arch=compute_75,code=sm_75>) 
# set_target_properties( somethingPTX
#                        PROPERTIES 
#                        CUDA_SEPARABLE_COMPILATION ON
# )

add_library(somethingLib SHARED ${cpp_source_files} ${cuda_source_files})
#target_compile_features(somethingLib PUBLIC cxx_std_11)
set_target_properties( somethingLib
                       PROPERTIES 
                       CUDA_SEPARABLE_COMPILATION ON
                       POSITION_INDEPENDENT_CODE ON
                       )
#set_property(TARGET somethingLib PROPERTY POSITION_INDEPENDENT_CODE ON)

#set_target_properties(somethingLib PROPERTIES POSITION_INDEPENDENT_CODE ON)
#target_compile_options(something PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:--ptxas-options=-v -gencode arch=compute_75,code=sm_75>) 
target_compile_options(somethingLib PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:
    --ptxas-options=-v 
    -gencode arch=compute_75,code=sm_75> 
    --keep
    ) 

#add_executable(something2 ${SOMETHING_SOURCE_DIR}/ptxjit/main.cu)
#set_target_properties(somethingLib PROPERTIES
 #                           CUDA_SEPERABLE_COMPILATION ON)
#target_compile_options(somethingLib PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:--ptxas-options=-v -rdc=true -gencode arch=compute_75,code=sm_75> ) 
#set_property(TARGET somethingLib PROPERTY CUDA_PTX_COMPILATION ON)
#set_property(TARGET somethingPTX PROPERTY CUDA_PTX_COMPILATION ON)

#target_compile_options(somethingLib PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:
#                       --generate-line-info
#                       --use_fast_math
#                       --relocatable-device-code=true
                       #--compiler-options '-fPIC'
                       #--fPIC
#                       >)
                       
                     
add_executable(something ${SOMETHING_SOURCE_DIR}/prog/main.cu)
#target_compile_options(something PRIVATE --fPIC)
set_target_properties(something PROPERTIES POSITION_INDEPENDENT_CODE ON)

#target_compile_options(something PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:
#                        --generate-line-info
#                        --use_fast_math
#                        --relocatable-device-code=true
#                        --compiler-options '-fPIC'
#                        >)

set_property(TARGET something 
             PROPERTY CUDA_SEPARABLE_COMPILATION ON)
target_link_libraries( something
    somethingLib
)



