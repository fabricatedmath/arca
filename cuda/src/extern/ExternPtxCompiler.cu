#include "ExternPtxCompiler.cuh"

extern "C" {
    PtxCompiler* ptxCompilerNew() {
        return new PtxCompiler();
    }

    int ptxCompilerCompile(PtxCompiler* ptxCompiler, const char* str, const int strLen, bool rdc) {
        return ptxCompiler->compile(str,strLen,rdc);
    }

    char* ptxCompilerGetPtxStr(PtxCompiler* ptxCompiler) {
        return ptxCompiler->getPtxStr();
    }

    int ptxCompilerGetPtxStrLen(PtxCompiler* ptxCompiler) {
        return ptxCompiler->getPtxStrLen();
    }

    char* ptxCompilerGetLogStr(PtxCompiler* ptxCompiler) {
        return ptxCompiler->getLogStr();
    }

    int ptxCompilerGetLogStrLen(PtxCompiler* ptxCompiler) {
        return ptxCompiler->getLogStrLen();
    }

    void ptxCompilerDelete(PtxCompiler* ptxCompiler) {
        delete ptxCompiler;
    }
}