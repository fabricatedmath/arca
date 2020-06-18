#pragma once

#include "NvrtcContainer.cuh"

extern "C" {
    NvrtcContainer* nvrtcContainerNew();
    void nvrtcContainerInit();
    bool nvrtcContainerCompile(NvrtcContainer* nvrtcContainer, const char* str, const int strlen);
    void nvrtcContainerRun(NvrtcContainer* nvrtcContainer);
    void nvrtcContainerDelete(NvrtcContainer* nvrtcContainer);
}