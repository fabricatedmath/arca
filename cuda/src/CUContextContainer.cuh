#pragma once

#include <cuda.h>

class CUContextContainer {
private:
    CUcontext* cuContext;
public:
    CUContextContainer();
    CUcontext* getCuContext();
    void setCurrentContext();
    void popContext();
    ~CUContextContainer();
};