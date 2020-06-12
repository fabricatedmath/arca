#include "device.cuh"

#include <iostream>

__device__
int device_call() {
    printf("dogs\n");
    return 5;
}

