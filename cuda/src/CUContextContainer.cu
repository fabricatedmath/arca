#include "CUContextContainer.cuh"

#define CUDA_DRIVER_API

#include <HelperCuda.h>

CUContextContainer::CUContextContainer() {
    checkCudaErrors( cuInit(0) );
    CUdevice cuDevice;
    checkCudaErrors( cuDeviceGet(&cuDevice, 0) );
    cuContext = new CUcontext();
    checkCudaErrors( cuCtxCreate(cuContext, 0, cuDevice) );
}

CUcontext* CUContextContainer::getCuContext() {
    return cuContext;
}

void CUContextContainer::setCurrentContext() {
    cuCtxSetCurrent(*cuContext);
}

void CUContextContainer::popContext() {
    cuCtxPopCurrent(NULL);
}

CUContextContainer::~CUContextContainer() {
    checkCudaErrors( cuCtxDestroy(*cuContext) );
    delete cuContext;
}