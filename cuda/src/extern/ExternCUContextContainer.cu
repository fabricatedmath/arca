#include "ExternCUContextContainer.cuh"

extern "C" {
    CUContextContainer* cuContextContainerNew() {
        return new CUContextContainer();
    }

    void cuContextContainerSetCurrentContext(CUContextContainer* cuContextContainer) {
        cuContextContainer->setCurrentContext();
    }

    void cuContextContainerPopContext(CUContextContainer* cuContextContainer) {
        cuContextContainer->popContext();
    }

    void cuContextContainerDelete(CUContextContainer* cuContextContainer) {
        delete cuContextContainer;
    }
}