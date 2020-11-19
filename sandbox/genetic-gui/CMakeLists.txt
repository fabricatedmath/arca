cmake_minimum_required(VERSION 3.18.4)

project(test_project LANGUAGES CUDA CXX)

set(APP_TARGET imgui-demo.app)

set(THIRD_PARTY_DIR ../../thirdparty)

include_directories(
    "${test_project_SOURCE_DIR}"
)

set(CUDASOURCE 
    ${test_project_SOURCE_DIR}/something.cu
)

set(CPPSOURCE 
    ${test_project_SOURCE_DIR}/transposer.cpp
)

add_subdirectory(${THIRD_PARTY_DIR} thirdparty)
include_directories(${THIRD_PARTY_DIR}/imgui)
include_directories(${THIRD_PARTY_DIR}/imgui/backends)
add_executable(${APP_TARGET} main.cpp ${CUDASOURCE} ${CPPSOURCE})
target_link_libraries(${APP_TARGET} imgui)

include_directories(${THIRD_PARTY_DIR}/implot)
target_link_libraries(${APP_TARGET} implot)

# glfw
find_package(glfw3 REQUIRED)
include_directories(${GLFW_INCLUDE_DIRS})
link_libraries(${GLFW_LIBRARY_DIRS})

# opengl
find_package(OpenGL REQUIRED)
include_directories(${OPENGL_INCLUDE_DIRS})

# glew
find_package(GLEW REQUIRED)
include_directories(${GLEW_INCLUDE_DIRS})

target_link_libraries(
    ${APP_TARGET}
    glfw
    ${OPENGL_LIBRARIES}
    ${GLEW_LIBRARIES}
)

