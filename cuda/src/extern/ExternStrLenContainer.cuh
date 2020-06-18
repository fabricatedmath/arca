#pragma once

#include "StrLenContainer.cuh"

extern "C" {
    char* strLenContainerGetPtr(StrLenContainer* strLenContainer);
    int strLenContainerGetLen(StrLenContainer* strLenContainer);
}