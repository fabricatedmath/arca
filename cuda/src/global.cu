#include "global.cuh"
#include "device.cuh"

#include <iostream>

__global__
void kernel() {
    int x = device_call();
    if (threadIdx.x == 0) {
        printf("%d\n",x);
    }
}

void call() {
    kernel<<<1,32>>>();
    cudaDeviceSynchronize();
}

