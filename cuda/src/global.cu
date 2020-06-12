#include "global.cuh"
#include "device.cuh"

#include <iostream>

#define checkCUDA(expression) {                               \
    cudaError_t status = (expression);                        \
    if (status != cudaSuccess) {                              \
      std::cerr << "Error on file: " << __FILE__ << ", line " << __LINE__ << ": "       \
                << cudaGetErrorString(status) << std::endl;   \
      std::exit(EXIT_FAILURE);                                \
    }                                                         \
}

__global__
void kernel() {
    int x = device_call();
    if (threadIdx.x == 0) {
        printf("%d\n",x);
    }
}

void call() {
    kernel<<<1,32>>>();
    checkCUDA( cudaGetLastError() );
    checkCUDA( cudaDeviceSynchronize() );
}

