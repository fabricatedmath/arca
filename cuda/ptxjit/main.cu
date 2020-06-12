#include <iostream>

#include <cuda.h>
#include <cuda_runtime.h>

#include "test.cuh"

using namespace std;

int main() {
    cout << "dogs" << endl;
    //CUlinkState lState;
    kernel<<<1,32>>>();
    cudaDeviceSynchronize();

}