#pragma once

#include <cuda.h>
#include <cuda_runtime.h>

class NvrtcContainer {
private:
    CUmodule hModule;
    CUfunction hKernel;
    //nvrtcProgram prog;
public:
    NvrtcContainer();
    static void init();
    bool compile(const char* str, const int strlen);
    void run();
    ~NvrtcContainer();
};