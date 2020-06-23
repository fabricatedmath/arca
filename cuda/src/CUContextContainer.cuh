#pragma once

#include <cuda.h>

class CUContextContainer {
private:
    CUcontext* ctx;
public:
    CUContextContainer();
    CUcontext* getCtx();
    ~CUContextContainer();
};