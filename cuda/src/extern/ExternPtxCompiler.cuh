#pragma once

#include "PtxCompiler.cuh"

extern "C" {
    PtxCompiler* ptxCompilerNew();
    int ptxCompilerCompile(PtxCompiler* ptxCompiler, const char* str, const int strLen, bool rdc=false);
    char* ptxCompilerGetPtxStr(PtxCompiler* ptxCompiler);
    int ptxCompilerGetPtxStrLen(PtxCompiler* ptxCompiler);
    char* ptxCompilerGetLogStr(PtxCompiler* ptxCompiler);
    int ptxCompilerGetLogStrLen(PtxCompiler* ptxCompiler);
    void ptxCompilerDelete(PtxCompiler* ptxCompiler);
}