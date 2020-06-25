#include "PtxLinker.cuh"

#include <iostream>

using namespace std;

#define CUDA_DRIVER_API

#include <helper_cuda.h>

#include <chrono>
using namespace std::chrono;

#define CURESULT_SAFE_CALL(x)                                \
  do {                                                          \
    CUresult result = x;                                     \
    if (result != CUDA_SUCCESS) {                              \
      return result;                                                  \
    }                                                           \
  } while (0)

PtxLinker::PtxLinker() 
    : hModule(0), hKernel(0) {}

int PtxLinker::link(const char* ptx, const int ptxLen, const char* funcNameStr, const int funcNameStrLen) {
    auto t0 = steady_clock::now();
    const string funcName(funcNameStr,funcNameStrLen);

    CUlinkState lState;
    
    CUjit_option options[6];
    void *optionVals[6];

    float walltime;
    char error_log[8192], info_log[8192];
    unsigned int logSize = 8192;

    // Setup linker options
    // Return walltime from JIT compilation
    options[0] = CU_JIT_WALL_TIME;
    optionVals[0] = (void *)&walltime;
    // Pass a buffer for info messages
    options[1] = CU_JIT_INFO_LOG_BUFFER;
    optionVals[1] = (void *)info_log;
    // Pass the size of the info buffer
    options[2] = CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES;
    optionVals[2] = (void *)(long)logSize;
    // Pass a buffer for error message
    options[3] = CU_JIT_ERROR_LOG_BUFFER;
    optionVals[3] = (void *)error_log;
    // Pass the size of the error buffer
    options[4] = CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES;
    optionVals[4] = (void *)(long)logSize;
    // Make the linker verbose
    options[5] = CU_JIT_LOG_VERBOSE;
    optionVals[5] = (void *)1;

    CUlinkState *plState = &lState;
    CURESULT_SAFE_CALL( cuLinkCreate(6, options, optionVals, plState) );
    //checkCudaErrors( cuLinkCreate(0, 0, 0, plState) );
    CURESULT_SAFE_CALL( cuLinkAddData(*plState, CU_JIT_INPUT_PTX, (void *)ptx, ptxLen, NULL, 0, 0, 0) );
    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    //printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    void* cuOut;
    size_t outSize;
    CUresult curesult = cuLinkComplete(*plState, &cuOut, &outSize);

    printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    if (curesult != CUDA_SUCCESS) {
        return curesult;
    }

    CURESULT_SAFE_CALL( cuModuleLoadData(&hModule, cuOut) );
    CURESULT_SAFE_CALL( cuModuleGetFunction(&hKernel, hModule, funcName.c_str()) );
    CURESULT_SAFE_CALL( cuLinkDestroy(*plState) );

    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    //printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    auto t1 = steady_clock::now();

    cout << duration_cast<milliseconds>(t1-t0).count() << endl;
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

PtxLinker::~PtxLinker() {
    if (hModule) {
        checkCudaErrors( cuModuleUnload(hModule) );
        hModule = 0;
    }
}