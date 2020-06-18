#include "ExternNvrtcContainer.cuh"

extern "C" {
    NvrtcContainer* nvrtcContainerNew() {
        return new NvrtcContainer();
    }

    void nvrtcContainerInit() {
        NvrtcContainer::init();
    }

    bool nvrtcContainerCompile(NvrtcContainer* nvrtcContainer, const char* str, const int strLen) {
        return nvrtcContainer->compile(str,strLen);
    }

    void nvrtcContainerRun(NvrtcContainer* nvrtcContainer) {
        return nvrtcContainer->run();
    }

    void nvrtcContainerDelete(NvrtcContainer* nvrtcContainer) {
        delete nvrtcContainer;
    }
}