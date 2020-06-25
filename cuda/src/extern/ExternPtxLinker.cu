#include "ExternPtxLinker.cuh"

extern "C" {
    PtxLinker* ptxLinkerNew() {
        return new PtxLinker();
    }

    int ptxLinkerLink(PtxLinker* ptxLinker, const char* str, const int strLen, const char* funcNameStr, const int funcNameStrLen) {
        return ptxLinker->link(str,strLen,funcNameStr,funcNameStrLen);
    }

    int ptxLinkerRun(PtxLinker* ptxLinker, const int numBlocks, const int numThreads) {
        return ptxLinker->run(numBlocks, numThreads);
    }

    void ptxLinkerDelete(PtxLinker* ptxLinker) {
        delete ptxLinker;
    }
}