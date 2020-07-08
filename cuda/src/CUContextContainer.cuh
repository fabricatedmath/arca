#pragma once

#include <cuda.h>

class CUContextContainer {
private:
    CUcontext* cuContext;
public:
    CUContextContainer();
    CUContextContainer(int deviceNum);
    CUcontext* getCuContext();
    void setCurrentContext();
    void popContext();
    ~CUContextContainer();
};