#include <iostream>

using namespace std;

const int numStreams = 1000;

__global__
void kernel(const int streamNum) {
    if (threadIdx.x == 0) {
        printf("stream %d, waiting\n", streamNum);
    }
    while(true);
}

int main() {
    cout << "dogs" << endl;

    cudaStream_t streams[numStreams];

    for(int i = 0; i < numStreams; i++) {
        cudaStreamCreate(&streams[i]);
        kernel<<<1,32,0,streams[i]>>>(i);
    }
    cudaDeviceSynchronize();
}