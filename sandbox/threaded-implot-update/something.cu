#include <iostream>

using namespace std;

__global__ 
void kernel() {
    if (threadIdx.x == 0 && blockIdx.x == 0) {
        printf("Kernel\n");
    }
}

void dogs() {
    kernel<<<1,1>>>();
    cudaDeviceSynchronize();
    cout << "dogs done" << endl;
}