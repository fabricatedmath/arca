#pragma once

#include <cuda.h>
#include <cuda_runtime.h>

#include <nvrtc.h>

#include "CUContextContainer.cuh"

class NvrtcContainer2 {
private:
    CUmodule hModule;
    CUfunction hKernel;
public:
    NvrtcContainer2(CUContextContainer* cuContextContainer);
    static void init();
    bool compile(const char* str, const int strlen);
    void run(const int numBlocks, const int numThreads);
    ~NvrtcContainer2();
};