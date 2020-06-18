#include "ExternCUContextContainer.cuh"

extern "C" {
    CUContextContainer* cuContextContainerNew() {
        return new CUContextContainer();
    }

    CUcontext* cuContextContainerGetCtx(CUContextContainer* cuContextContainer) {
        return cuContextContainer->getCtx();
    }

    void cuContextContainerDelete(CUContextContainer* cuContextContainer) {
        delete cuContextContainer;
    }
}