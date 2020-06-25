#pragma once

#include "PtxLinker.cuh"

extern "C" {
    PtxLinker* ptxLinkerNew();
    int ptxLinkerLink(PtxLinker* ptxLinker, const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen);
    int ptxLinkerRun(PtxLinker* ptxLinker, const int numBlocks, const int numThreads);
    void ptxLinkerDelete(PtxLinker* ptxLinker);
}