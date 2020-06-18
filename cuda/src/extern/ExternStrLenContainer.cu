#include "ExternStrLenContainer.cuh"

extern "C" {
    char* strLenContainerGetPtr(StrLenContainer* strLenContainer) {
        return strLenContainer->getStrPtr();
    }

    int strLenContainerGetLen(StrLenContainer* strLenContainer) {
        return strLenContainer->getStrLen();
    }
}