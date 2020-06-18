#include "ExternNvrtcContainer.cuh"

extern "C" {
    NvrtcContainer* nvrtcContainerNew(CUContextContainer* cuContextContainer) {
        return new NvrtcContainer(cuContextContainer);
    }

    void nvrtcContainerInit() {
        NvrtcContainer::init();
    }

    bool nvrtcContainerCompile(NvrtcContainer* nvrtcContainer, const char* str, const int strLen) {
        return nvrtcContainer->compile(str,strLen);
    }

    void nvrtcContainerRun(NvrtcContainer* nvrtcContainer, const int numBlocks, const int numThreads) {
        return nvrtcContainer->run(numBlocks, numThreads);
    }

    void nvrtcContainerDelete(NvrtcContainer* nvrtcContainer) {
        delete nvrtcContainer;
    }
}