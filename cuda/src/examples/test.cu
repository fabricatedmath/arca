#include "test.cuh"

#include <iostream>

namespace {
    __device__ 
    void call2() {
        if(threadIdx.x == 0) {
            printf("doggos\n");
        }
    }
}

__global__
void kernel2() {
    if (threadIdx.x == 0) {
        printf("camels\n");
    }
    call2();
}