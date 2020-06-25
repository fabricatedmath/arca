#pragma once

#include <cuda.h>
#include <cuda_runtime.h>

class PtxLinker {
private:
    CUmodule hModule;
    CUfunction hKernel;
public:
    PtxLinker();
    int link(const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen);
    int run(const int numBlocks, const int numThreads);
    ~PtxLinker();
};