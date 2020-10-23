#include <chrono>
#include <iostream>
#include <cooperative_groups.h>
#include <curand_kernel.h>

using namespace std;

using namespace cooperative_groups;

#define TIME_INC 100000000
#define INCS 10
#define USE_PROGRESS 1
#define MAT_DIMX 4000
#define MAT_DIMY MAT_DIMX

#define checkCudaErrors( err ) \
  if( err != cudaSuccess ) { \
    std::cerr << "ERROR: " << cudaGetErrorString( err ) << std::endl; \
    exit( -1 ); \
  }


__global__
void kernel(volatile int* data) {
    atomicAdd((int*)data,1);
    //long long int start2 = clock64();
    //int start = clock();
    //data[threadIdx.x] = 1;
    //__threadfence_system();
    //int end = clock();
    //data[threadIdx.x+32] = start2; //end - start;
}

__global__
void mykernel(volatile int* notify, volatile int* notify2, volatile float* d_buffer) {
    float v = threadIdx.x;
    unsigned long time;
    for (int i = 0; i < INCS; i++){
        if(threadIdx.x == 0) {
            atomicAdd((int *)notify+blockIdx.x,1);
        }

        v++;
        d_buffer[threadIdx.x] = v;
        
        if(threadIdx.x == 0) {
            atomicAdd((int *)notify2+blockIdx.x,1);
        }

        __threadfence_system();
        time = clock64();
        while((clock64() - time)<TIME_INC) {};
    }
    if (threadIdx.x == 0 && blockIdx.x == 0) {
        printf("progress check finished\n");
    }
}

const int numBlocks = 1;
const int numThreads = 32;

const int numData = numBlocks * numThreads;
const size_t numBytesData = numData * sizeof(float);

const int numNotify = numBlocks;
const size_t numBytesNotify = numBlocks * sizeof(int);

const int numIntermediates = numThreads;
const size_t numBytesIntermediateBuffer = numIntermediates * sizeof(float);

int main() {
    cudaStream_t mainStream, asyncCopyStream;
    cudaStreamCreate( &mainStream );
    cudaStreamCreate( &asyncCopyStream );

    float* d_data;
    checkCudaErrors( cudaMalloc((void **)&d_data, numBytesData) );
    float* h_data = (float *)malloc(numBytesData);

    // simple test to demonstrate reading progress data from kernel
    checkCudaErrors( cudaSetDeviceFlags(cudaDeviceMapHost) );

    volatile int *d_notify, *h_notify;
    checkCudaErrors( cudaHostAlloc((void **)&h_notify, numBytesNotify, cudaHostAllocMapped) );
    checkCudaErrors( cudaHostGetDevicePointer((int **)&d_notify, (int *)h_notify, 0) );
    for(int i = 0; i < numNotify; i++) {
        h_notify[i] = 0;
    }

    volatile int *d_notify2, *h_notify2;
    checkCudaErrors( cudaSetDeviceFlags(cudaDeviceMapHost) );
    checkCudaErrors( cudaHostAlloc((void **)&h_notify2, numBytesNotify, cudaHostAllocMapped) );
    checkCudaErrors( cudaHostGetDevicePointer((int **)&d_notify2, (int *)h_notify2, 0) );
    for(int i = 0; i < numNotify; i++) {
        h_notify2[i] = 0;
    }

    float* h_intermediate = (float*)malloc(numBytesIntermediateBuffer);

    printf("kernel starting\n");
    mykernel<<<numBlocks,numThreads,0,mainStream>>>(d_notify, d_notify2, d_data);
    checkCudaErrors( cudaGetLastError() );

    int value = 0;
    do{
        int value1 = *h_notify;
        if (value1 > value){
        checkCudaErrors( cudaMemcpyAsync(h_intermediate, d_data, numBytesIntermediateBuffer, cudaMemcpyDeviceToHost, asyncCopyStream) );
        for (int i = 0; i < numIntermediates; i++) {
            cout << h_intermediate[i] << ",";
        }
        cout << endl;
        printf("h_data = %d\n", value1);

        value = value1;}}
        while (value < (INCS-1));
    cudaDeviceSynchronize();

    checkCudaErrors( cudaMemcpy(h_data, d_data, numBytesData, cudaMemcpyDeviceToHost) );
    for (int i = 0; i < numData; i++) {
        cout << h_data[i] << ",";
    }
    cout << endl;

  /*
    curandStatePhilox4_32_10_t *devPHILOXStates;
    checkCudaErrors( cudaMalloc((void **)&devPHILOXStates, totalThreads * sizeof(curandStatePhilox4_32_10_t)) );

    setup_kernel<<<dimGrid,dimBlock>>>(devPHILOXStates);
    checkCudaErrors( cudaGetLastError() );
    checkCudaErrors( cudaDeviceSynchronize() );

    unsigned int* h_mem = (unsigned int *)malloc(numBytes);
    unsigned int* d_mem;
    checkCudaErrors( cudaMalloc((void **) &d_mem, numBytes) );

    initializePopulations<<<dimGrid,dimBlock>>>(d_mem, devPHILOXStates);
    checkCudaErrors( cudaGetLastError() );
    checkCudaErrors( cudaDeviceSynchronize() );

    void *kernelArgs[] = {
        (void*)&d_mem,
        (void*)&devPHILOXStates
    };

    cudaMemcpyToSymbol(c_poly, &pPoly, numPolys * sizeof(int));

    checkCudaErrors( cudaLaunchCooperativeKernel((void*)testKernel, dimGrid, dimBlock, kernelArgs, smemSizeBytes, NULL) );
    checkCudaErrors( cudaGetLastError() );
    checkCudaErrors( cudaDeviceSynchronize() );

    checkCudaErrors( cudaMemcpy(h_mem, d_mem, numBytes, cudaMemcpyDeviceToHost) );

    for (int block = 0; block < numBlocks; block++) {
        cout << "block: " << block << endl;
        for (int pop = 0; pop < smemPopNum; pop++) {
            cout << "\t pop:" << pop << endl;
            for (int i = 0; i < 32; i++) {
                cout << h_mem[block * smemPopNum * 32 + pop * 32 + i] << ',';
            }
            cout << endl;
        }
    }
    */

}