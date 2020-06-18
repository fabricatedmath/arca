#include "NvrtcContainer.cuh"

#include <iostream>

#include <nvrtc.h>

#define CUDA_DRIVER_API

#include <helper_cuda.h>

using namespace std;

#define NVRTC_SAFE_CALL(Name, x)                                \
  do {                                                          \
    nvrtcResult result = x;                                     \
    if (result != NVRTC_SUCCESS) {                              \
      std::cerr << "\nerror: " << Name << " failed with error " \
                << nvrtcGetErrorString(result);                 \
      exit(1);                                                  \
    }                                                           \
  } while (0)

NvrtcContainer::NvrtcContainer() : hModule(0), hKernel(0) {}

void NvrtcContainer::init() {
    checkCudaErrors( cuInit(0) );
}

bool NvrtcContainer::compile(const char* str, const int strlen) {
    nvrtcProgram prog;
    NVRTC_SAFE_CALL("nvrtcCreateProgram", nvrtcCreateProgram(&prog, str, "device.cu", 0, NULL, NULL) );
    const char *opts[] = {}; //{"--ptxas-options -v"}; //{"-rdc=true", "--ptxas-options -v"};
    nvrtcResult compileResult = nvrtcCompileProgram(prog, 0, opts); 

    size_t logSize2;
    NVRTC_SAFE_CALL("nvrtcGetProgramLogSize", nvrtcGetProgramLogSize(prog, &logSize2) );
    char *log = new char[logSize2];
    NVRTC_SAFE_CALL("nvrtcGetProgramLog", nvrtcGetProgramLog(prog, log) );
    std::cout << log << '\n';
    delete[] log;

    if (compileResult != NVRTC_SUCCESS) {
      cout << "failed to compile" << endl;
      return false;
    }

    size_t ptxSize;
    NVRTC_SAFE_CALL( "nvrtcGetPTXSize", nvrtcGetPTXSize(prog, &ptxSize) );
    char *ptx = new char[ptxSize];
    NVRTC_SAFE_CALL( "nvrtcGetPTX", nvrtcGetPTX(prog, ptx) );
    cout << ptx << endl;

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

    //checkCudaErrors( cuInit(0) );
    //cudaSetDevice(0);
    CUdevice cuDevice;
    checkCudaErrors( cuDeviceGet(&cuDevice, 0) );
    CUcontext context;
    checkCudaErrors( cuCtxCreate(&context, 0, cuDevice) );
    checkCudaErrors( cuLinkCreate(6, options, optionVals, plState) );
    checkCudaErrors( cuLinkAddData(*plState, CU_JIT_INPUT_PTX, (void *)ptx, ptxSize+1, "device.ptx", 0, 0, 0) );
    printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);
    
    delete ptx;

    printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    void* cuOut;
    size_t outSize;
    CUresult curesult = cuLinkComplete(*plState, &cuOut, &outSize);

    printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    if (curesult != CUDA_SUCCESS) {
        return false;
    }

    checkCudaErrors( cuModuleLoadData(&hModule, cuOut) );
    checkCudaErrors( cuModuleGetFunction(&hKernel, hModule, "kernel") );
    checkCudaErrors( cuLinkDestroy(*plState) );

    printf("CUDA Link Completed in %fms. Linker Output:\n%s\n", walltime, info_log);

    printf("CUDA Link Completed in %fms. Linker Error Output:\n%s\n", walltime, error_log);

    return true;
}

void NvrtcContainer::run() {
    int nThreads = 32;
    int nBlocks = 1;
    dim3 block(nThreads, 1, 1);
    dim3 grid(nBlocks, 1, 1);
  
    void *args[0] = {};
  
    checkCudaErrors( cuLaunchKernel(hKernel, grid.x, grid.y, grid.z, block.x, block.y, block.z, 0, NULL, args, NULL) );
    cudaDeviceSynchronize();
}

NvrtcContainer::~NvrtcContainer() {
    if (hModule) {
        checkCudaErrors( cuModuleUnload(hModule) );
        hModule = 0;
    }
}