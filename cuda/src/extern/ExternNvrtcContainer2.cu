#include "ExternNvrtcContainer2.cuh"

extern "C" {
    NvrtcContainer2* nvrtcContainerNew(CUContextContainer* cuContextContainer) {
        return new NvrtcContainer2(cuContextContainer);
    }

    void nvrtcContainerInit() {
        NvrtcContainer2::init();
    }

    bool nvrtcContainerCompile(NvrtcContainer2* nvrtcContainer, const char* str, const int strLen) {
        return nvrtcContainer->compile(str,strLen);
    }

    void nvrtcContainerRun(NvrtcContainer2* nvrtcContainer, const int numBlocks, const int numThreads) {
        return nvrtcContainer->run(numBlocks, numThreads);
    }

    void nvrtcContainerDelete(NvrtcContainer2* nvrtcContainer) {
        delete nvrtcContainer;
    }
}