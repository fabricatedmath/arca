#pragma once

#include "NvrtcContainer2.cuh"

extern "C" {
    NvrtcContainer2* nvrtcContainerNew(CUContextContainer* cuContextContainer);
    void nvrtcContainerInit();
    bool nvrtcContainerCompile(NvrtcContainer2* nvrtcContainer, const char* str, const int strlen);
    void nvrtcContainerRun(NvrtcContainer2* nvrtcContainer, const int numBlocks, const int numThreads);
    void nvrtcContainerDelete(NvrtcContainer2* nvrtcContainer);
}