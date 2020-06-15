#include "device.cuh"

#include <iostream>

__device__
int device_call() {
    if (threadIdx.x == 0) {
        printf("cats\n");
    }
    return 7;
}

