#pragma once

#include "CUContextContainer.cuh"

extern "C" {
    CUContextContainer* cuContextContainerNew();
    CUcontext* cuContextContainerGetCtx(CUContextContainer* cuContextContainer);
    void cuContextContainerDelete(CUContextContainer* cuContextContainer);
}