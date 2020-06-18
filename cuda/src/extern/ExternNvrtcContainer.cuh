#pragma once

#include "NvrtcContainer.cuh"

extern "C" {
    NvrtcContainer* nvrtcContainerNew(CUContextContainer* cuContextContainer);
    void nvrtcContainerInit();
    bool nvrtcContainerCompile(NvrtcContainer* nvrtcContainer, const char* str, const int strlen);
    void nvrtcContainerRun(NvrtcContainer* nvrtcContainer, const int numBlocks, const int numThreads);
    void nvrtcContainerDelete(NvrtcContainer* nvrtcContainer);
}