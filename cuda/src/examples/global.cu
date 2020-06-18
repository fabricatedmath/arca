#include "global.cuh"
#include "device.cuh"

#include <iostream>

#include <helper_cuda.h>

using namespace std;

extern "C" {
    __global__
    void kernel() {
        int x = device_call();
        if (threadIdx.x == 0) {
            printf("%d\n",x);
        }
    }
}

void call() {
    kernel<<<1,32>>>();
    checkCudaErrors( cudaGetLastError() );
    checkCudaErrors( cudaDeviceSynchronize() );
}

