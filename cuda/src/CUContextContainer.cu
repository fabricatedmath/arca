#include "CUContextContainer.cuh"

#define CUDA_DRIVER_API

#include <helper_cuda.h>

CUContextContainer::CUContextContainer() {
    checkCudaErrors( cuInit(0) );
    CUdevice cuDevice;
    checkCudaErrors( cuDeviceGet(&cuDevice, 0) );
    ctx = new CUcontext();
    checkCudaErrors( cuCtxCreate(ctx, 0, cuDevice) );
}

CUcontext* CUContextContainer::getCtx() {
    return ctx;
}

CUContextContainer::~CUContextContainer() {
    checkCudaErrors( cuCtxDestroy(*ctx) );
    delete ctx;
}