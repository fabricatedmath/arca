#pragma once

#include "PtxLinker.cuh"

extern "C" {
    PtxLinker* ptxLinkerNew();
    int ptxLinkerLink(PtxLinker* ptxLinker, const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen);
    int ptxLinkerRun(PtxLinker* ptxLinker, const int numBlocks, const int numThreads);
    char* ptxLinkerGetInfoLogStr(PtxLinker* ptxLinker);
    int ptxLinkerGetInfoLogStrLen(PtxLinker* ptxLinker);
    char* ptxLinkerGetErrorLogStr(PtxLinker* ptxLinker);
    int ptxLinkerGetErrorLogStrLen(PtxLinker* ptxLinker);
    void ptxLinkerDelete(PtxLinker* ptxLinker);
}