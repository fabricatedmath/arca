#include "PtxCompiler.cuh"

#include <iostream>

using namespace std;

#include <nvrtc.h>

#define NVRTC_SAFE_CALL(Name, x)                                \
  do {                                                          \
    nvrtcResult result = x;                                     \
    if (result != NVRTC_SUCCESS) {                              \
      std::cerr << "\nerror: " << Name << " failed with error " \
                << nvrtcGetErrorString(result);                 \
      exit(1);                                                  \
    }                                                           \
  } while (0)

PtxCompiler::PtxCompiler() : ptxStr(NULL), ptxStrLen(0), logStr(NULL), logStrLen(0) {}

int PtxCompiler::compile(const char* str, const int strLen, bool rdc) {
    const string progStr(str,strLen);
    nvrtcProgram prog;
    NVRTC_SAFE_CALL("nvrtcCreateProgram", nvrtcCreateProgram(&prog, progStr.c_str(), NULL, 0, NULL, NULL) );
    nvrtcResult compileResult;

    if (rdc) {
        const char *opts[] = {"-rdc=true", "--gpu-architecture=compute_75"};
        compileResult = nvrtcCompileProgram(prog, 2, opts); 
    } else {
        const char *opts[] = {"--gpu-architecture=compute_75"};
        compileResult = nvrtcCompileProgram(prog, 1, opts); 
    }

    NVRTC_SAFE_CALL("nvrtcGetProgramLogSize", nvrtcGetProgramLogSize(prog, &logStrLen) );
    logStr = new char[logStrLen];
    NVRTC_SAFE_CALL("nvrtcGetProgramLog", nvrtcGetProgramLog(prog, logStr) );

    if (compileResult != NVRTC_SUCCESS) {
        return compileResult;
    }

    NVRTC_SAFE_CALL( "nvrtcGetPTXSize", nvrtcGetPTXSize(prog, &ptxStrLen) );
    ptxStr = new char[ptxStrLen];
    NVRTC_SAFE_CALL( "nvrtcGetPTX", nvrtcGetPTX(prog, ptxStr) );
    return compileResult;
}

char* PtxCompiler::getPtxStr() {
    return ptxStr;
}

size_t PtxCompiler::getPtxStrLen() {
    return ptxStrLen;
}

char* PtxCompiler::getLogStr() {
    return logStr;
}

size_t PtxCompiler::getLogStrLen() {
    return logStrLen;
}

PtxCompiler::~PtxCompiler() {
    if (ptxStr != NULL) {
        delete ptxStr;
    }

    if (logStr != NULL) {
        delete logStr;
    }
}