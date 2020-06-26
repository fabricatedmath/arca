#include <iostream>

#include <cuda.h>
#include <cuda_runtime.h>
#include <nvrtc.h>

using namespace std;

#include "ExternCUContextContainer.cuh"
#include "ExternPtxCompiler.cuh"
#include "ExternPtxLinker.cuh"

#define CUDA_DRIVER_API

#include <HelperCuda.h>

const char *cudaCodeStr = "                                           \n\
extern \"C\" __global__                                         \n\
void kernel()   \n\
{                                                               \n\
  size_t tid = blockIdx.x * blockDim.x + threadIdx.x;           \n\
  if (tid == 0) {                                                \n\
    printf(\"dogs:%d\\n\", tid);                             \n\
  }                                                             \n\
}                                                               \n";

const int cudaCodeStrLen = strlen(cudaCodeStr);

const char* funcNameStr = "kernel";

const int funcNameStrLen = strlen(funcNameStr);

const char* malformedPtxStr = "dogs";
const int malformedPtxStrLen = strlen(malformedPtxStr);

int main() {
  CUContextContainer* cuContextContainer = cuContextContainerNew();
  cuContextContainerSetCurrentContext(cuContextContainer);
  PtxCompiler* ptxCompiler = ptxCompilerNew();
  int result;
  result = ptxCompilerCompile(ptxCompiler, cudaCodeStr, cudaCodeStrLen, false);

  {
    const char* ptxCompilerLogStr = ptxCompilerGetLogStr(ptxCompiler);
    const int ptxCompilerLogStrLen = ptxCompilerGetLogStrLen(ptxCompiler);
    const string logStr(ptxCompilerLogStr,ptxCompilerLogStrLen);
    cout << "Compile Log: " << "\n" << logStr << endl;

  }

  if (result == NVRTC_SUCCESS) {
    const char* ptxStr = ptxCompilerGetPtxStr(ptxCompiler);
    const int ptxStrLen = ptxCompilerGetPtxStrLen(ptxCompiler);

    PtxLinker* ptxLinker = ptxLinkerNew();

    result = ptxLinkerLink(ptxLinker, ptxStr, ptxStrLen, funcNameStr, funcNameStrLen);
    if (result == CUDA_SUCCESS) {
      {
        const char* ptxLinkerLogStr = ptxLinkerGetInfoLogStr(ptxLinker);
        const int ptxLinkerLogStrLen = ptxLinkerGetInfoLogStrLen(ptxLinker);
        const string logStr(ptxLinkerLogStr,ptxLinkerLogStrLen);
        cout << "Linker Log: " << "\n" << logStr << "\n" << endl;
      }
      result = ptxLinkerRun(ptxLinker, 1, 32);
      if (result == CUDA_SUCCESS) {
        cout << "Successful run!" << endl;
      } else {
        CUresult cuResult = (CUresult)result;
        cout << "Failed run! Code: " << cuResult << "\n" << _cudaGetErrorEnum(cuResult) << endl;
      }
    } else {
        const char* ptxLinkerErrorStr = ptxLinkerGetErrorLogStr(ptxLinker);
        const int ptxLinkerErrorStrLen = ptxLinkerGetErrorLogStrLen(ptxLinker);
        const string errorStr(ptxLinkerErrorStr,ptxLinkerErrorStrLen);
        cout << "Linker Error Log: " << "\n" << errorStr << "\n" << endl;
    }
    ptxLinkerDelete(ptxLinker);
  }
  ptxCompilerDelete(ptxCompiler);
  cuContextContainerDelete(cuContextContainer);
}