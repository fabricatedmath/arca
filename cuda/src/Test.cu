#include "Test.cuh"

#include <stdio.h>

namespace {
    __global__
    void kernel() {
        for (int i = 0; i < 32; i++) {
            if (threadIdx.x == i) {
                printf("%d\n", threadIdx.x);
            }
        }
    }
}

void Test::call() {
    kernel<<<1,32>>>();
    cudaDeviceSynchronize();
}