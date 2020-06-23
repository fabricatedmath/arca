#pragma once

class PtxLinker {
private:
    CUmodule hModule;
    CUfunction hKernel;
public:
    PtxLinker(CUContextContainer* cuContextContainer);
    int link(const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen);
    void run(const int numBlock, const int numThreads);
    ~PtxLinker();
};