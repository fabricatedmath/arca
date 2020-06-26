#pragma once

#include <cuda.h>
#include <cuda_runtime.h>

class PtxLinker {
private:
    const int logSize = 8192;
    CUmodule hModule;
    CUfunction hKernel;
    char* infoLogStr;
    char* errorLogStr;
public:
    PtxLinker();

    int link(const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen);
    int run(const int numBlocks, const int numThreads);

    char* getInfoLogStr();
    size_t getInfoLogStrLen();

    char* getErrorLogStr();
    size_t getErrorLogStrLen();

    ~PtxLinker();
};