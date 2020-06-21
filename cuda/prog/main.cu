#include <iostream>

#include <cuda.h>
#include <cuda_runtime.h>
#include <nvrtc.h>

using namespace std;

#include "global.cuh"

#define CUDA_DRIVER_API

#include <helper_cuda.h>

#define NVRTC_SAFE_CALL(Name, x)                                \
  do {                                                          \
    nvrtcResult result = x;                                     \
    if (result != NVRTC_SUCCESS) {                              \
      std::cerr << "\nerror: " << Name << " failed with error " \
                << nvrtcGetErrorString(result);                 \
      exit(1);                                                  \
    }                                                           \
  } while (0)

const char *saxpy = "                                           \n\
 __device__                                         \n\
int device_call()                                               \n\
{                                                               \n\
    if (threadIdx.x == 0) {                                     \n\
        printf(\"llama\\n\");                                     \n\
    }                                                           \n\
    return 7;                                                    \n\
  } \n\
                                                              \n";

int main() {
  //call();
    nvrtcProgram prog;
    NVRTC_SAFE_CALL("nvrtcCreateProgram", nvrtcCreateProgram(&prog, saxpy, "device.cu", 0, NULL, NULL) );
    const char *opts[] = {"-rdc=true"};
    nvrtcResult compileResult = nvrtcCompileProgram(prog, 1, opts); 

    size_t logSize2;
    NVRTC_SAFE_CALL("nvrtcGetProgramLogSize", nvrtcGetProgramLogSize(prog, &logSize2) );
    char *log = new char[logSize2];
    NVRTC_SAFE_CALL("nvrtcGetProgramLog", nvrtcGetProgramLog(prog, log) );
    std::cout << log << '\n';
    delete[] log;

    if (compileResult != NVRTC_SUCCESS) {
      exit(1);
    }

    size_t ptxSize;
    NVRTC_SAFE_CALL( "nvrtcGetPTXSize", nvrtcGetPTXSize(prog, &ptxSize) );
    char *ptx = new char[ptxSize];
    NVRTC_SAFE_CALL( "nvrtcGetPTX", nvrtcGetPTX(prog, ptx) );
    //cout << ptx << endl;

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

    checkCudaErrors( cuInit(0) );
    //cudaSetDevice(0);
    CUdevice cuDevice;
    checkCudaErrors( cuDeviceGet(&cuDevice, 0) );
    CUcontext context;
    checkCudaErrors( cuCtxCreate(&context, 0, cuDevice) );
    checkCudaErrors( cuLinkCreate(6, options, optionVals, plState) );
    checkCudaErrors( cuLinkAddData(*plState, CU_JIT_INPUT_PTX, (void *)ptx, ptxSize+1, "device.ptx", 0, 0, 0) );
    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);
    checkCudaErrors( cuLinkAddFile(*plState, CU_JIT_INPUT_PTX, "build/global.ptx", 0, 0, 0) );
    //checkCudaErrors( cuLinkAddFile(*plState, CU_JIT_INPUT_PTX, "build/device.ptx", 0, 0, 0) );
    


    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    //printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    void* cuOut;
    size_t outSize;
    CUresult curesult = cuLinkComplete(*plState, &cuOut, &outSize);

    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    //printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    if (curesult != CUDA_SUCCESS) {
      exit(1);
    }

    CUmodule hModule = 0;
    CUfunction hKernel = 0;

    CUmodule* phModule = &hModule;
    CUfunction* phKernel = &hKernel;

    checkCudaErrors( cuModuleLoadData(phModule, cuOut) );
    checkCudaErrors( cuModuleGetFunction(phKernel, *phModule, "kernel") );
    checkCudaErrors( cuLinkDestroy(*plState) );

    //printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    //printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    int nThreads = 32;
    int nBlocks = 1;
    dim3 block(nThreads, 1, 1);
    dim3 grid(nBlocks, 1, 1);
  
    void *args[0] = {};
  
    checkCudaErrors( cuLaunchKernel(hKernel, grid.x, grid.y, grid.z, block.x, block.y, block.z, 0, NULL, args, NULL) );
    cudaDeviceSynchronize();

    if (hModule) {
        checkCudaErrors( cuModuleUnload(hModule) );
        hModule = 0;
    }
}