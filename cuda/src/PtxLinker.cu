#include "PtxLinker.cuh"

#include <iostream>

using namespace std;

#define CUDA_DRIVER_API

#include <HelperCuda.h>

#include <chrono>
using namespace std::chrono;

#define CURESULT_SAFE_CALL(x)         \
  do {                                \
    CUresult result = x;              \
    if (result != CUDA_SUCCESS) {     \
      return result;                  \
    }                                 \
  } while (0)

PtxLinker::PtxLinker() 
    : hModule(0)
    , hKernel(0)
    , infoLogStr(new char[logSize])
    , infoLogStrLen(0)
    , errorLogStr(new char[logSize])
    , errorLogStrLen(0) {}

int PtxLinker::link(const char* ptx, const int ptxLen, const char* funcNameStr, const int funcNameStrLen) {
    const string funcName(funcNameStr,funcNameStrLen);

    CUlinkState lState;
    
    CUjit_option options[6];
    void *optionVals[6];

    float walltime;
    //char error_log[8192], info_log[8192];
    //unsigned int logSize = 8192;

    // Setup linker options
    // Return walltime from JIT compilation
    options[0] = CU_JIT_WALL_TIME;
    optionVals[0] = (void *)&walltime;
    // Pass a buffer for info messages
    options[1] = CU_JIT_INFO_LOG_BUFFER;
    optionVals[1] = (void *)infoLogStr;
    // Pass the size of the info buffer
    options[2] = CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES;
    optionVals[2] = (void *)(long)logSize;
    // Pass a buffer for error message
    options[3] = CU_JIT_ERROR_LOG_BUFFER;
    optionVals[3] = (void *)errorLogStr;
    // Pass the size of the error buffer
    options[4] = CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES;
    optionVals[4] = (void *)(long)logSize;
    // Make the linker verbose
    options[5] = CU_JIT_LOG_VERBOSE;
    optionVals[5] = (void *)1;

    CURESULT_SAFE_CALL( cuLinkCreate(6, options, optionVals, &lState) );
    infoLogStrLen = logSize;
    errorLogStrLen = logSize;

    CURESULT_SAFE_CALL( cuLinkAddData(lState, CU_JIT_INPUT_PTX, (void *)ptx, ptxLen, NULL, 0, 0, 0) );

    void* cuOut;
    size_t outSize;
    CURESULT_SAFE_CALL( cuLinkComplete(lState, &cuOut, &outSize) );
    CURESULT_SAFE_CALL( cuModuleLoadData(&hModule, cuOut) );
    CURESULT_SAFE_CALL( cuModuleGetFunction(&hKernel, hModule, funcName.c_str()) );
    CURESULT_SAFE_CALL( cuLinkDestroy(lState) );
    return CUDA_SUCCESS;
}

int PtxLinker::run(const int numBlocks, const int numThreads) {
    int nThreads = numThreads;
    int nBlocks = numBlocks;
    dim3 block(nThreads, 1, 1);
    dim3 grid(nBlocks, 1, 1);
  
    void *args[0] = {};
  
    CURESULT_SAFE_CALL( cuLaunchKernel(hKernel, grid.x, grid.y, grid.z, block.x, block.y, block.z, 0, NULL, args, NULL) );
    cudaDeviceSynchronize();

    return CUDA_SUCCESS;
}

char* PtxLinker::getInfoLogStr() {
    return infoLogStr;
}

size_t PtxLinker::getInfoLogStrLen() {
    return infoLogStrLen;
}

char* PtxLinker::getErrorLogStr() {
    return errorLogStr;
}

size_t PtxLinker::getErrorLogStrLen() {
    return errorLogStrLen;
}

PtxLinker::~PtxLinker() {
    if (hModule) {
        checkCudaErrors( cuModuleUnload(hModule) );
        hModule = 0;
    }
    delete infoLogStr;
    delete errorLogStr;
}