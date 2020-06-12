#pragma once

#include "test2.cuh"

__global__ 
void kernel() {
    test2();
}