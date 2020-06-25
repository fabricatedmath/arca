#pragma once

#include "CUContextContainer.cuh"

extern "C" {
    CUContextContainer* cuContextContainerNew();
    void cuContextContainerSetCurrentContext(CUContextContainer* cuContextContainer);
    void cuContextContainerPopContext(CUContextContainer* cuContextContainer);
    void cuContextContainerDelete(CUContextContainer* cuContextContainer);
}