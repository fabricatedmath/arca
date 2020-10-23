cmake_minimum_required(VERSION 3.18.4)

include_directories(imgui)

add_library(
    imgui STATIC
    imgui/imgui.cpp
    imgui/imgui.h
    imgui/imgui_draw.cpp
    imgui/imgui_widgets.cpp
    imgui/imgui_demo.cpp
    imgui/backends/imgui_impl_opengl3.cpp
    imgui/backends/imgui_impl_opengl3.h
    imgui/backends/imgui_impl_glfw.cpp
    imgui/backends/imgui_impl_glfw.h
)

add_library(
    implot STATIC
    implot/implot.cpp
    implot/implot.h
    implot/implot_items.cpp
)
