#include <chrono>
#include <iostream>
#include <cooperative_groups.h>
#include <curand_kernel.h>

using namespace std;

using namespace cooperative_groups;

const int numPolys = 32;

__constant__ int c_poly[numPolys];

const int pPoly[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
//const int pPoly[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

#define checkCudaErrors( err ) \
  if( err != cudaSuccess ) { \
    std::cerr << "ERROR: " << cudaGetErrorString( err ) << std::endl; \
    exit( -1 ); \
  }

__global__ void setup_kernel(curandStatePhilox4_32_10_t *state) {
    const int id = threadIdx.x + blockIdx.x * blockDim.x;
    curand_init(1234, id, 0, &state[id]);
}
  
const unsigned mask = 0xFFFFFFFF;

__device__
int score(const int v) {
    const int c = c_poly[threadIdx.x];
    const int d = c ^ v;
    int s = __popc(d);
    for (int i=16; i>=1; i/=2) {
        s += __shfl_xor_sync(mask, s, i);
    }
    return s;
}

__device__
unsigned int mutationMask(curandStatePhilox4_32_10_t* localState) {
    const uint4 r1 = curand4(localState);
    const unsigned int v1 = r1.x & r1.y & r1.z & r1.w;
    const uint4 r2 = curand4(localState);
    const unsigned int v2 = r2.x & r2.y & r2.z & r2.w;
    return v1 & v2;
}

__device__ __inline__
unsigned int selectMask(unsigned int p1, unsigned int p2, unsigned int mask) {
    return (mask & p1) | (~mask & p2);
}

const int numThreads = 512;
const int numBlocks = 1;

// Actual shared memory available
// const size_t smemSizeBytes = 3 << 14;
const size_t smemSizeBytes = 1 << 15; // nearest power 2 size
const size_t smemNumFourBytes = smemSizeBytes / 4; // 12288
const size_t smemNumWarpLines = smemNumFourBytes / 32; // 384
const size_t smemPopNum = smemNumWarpLines / 2; // 192
const size_t smemNumWarps = numThreads / 32;

const int numElemsPerBlock = smemPopNum * 32;
const int numElems = numBlocks * numElemsPerBlock;
const size_t numBytes = numElems * sizeof(int);
const int intsPerPop = smemPopNum * 32;

const int totalThreads = numThreads * numBlocks;

__global__
void initializePopulations(unsigned int* d_mem, curandStatePhilox4_32_10_t* state) {
    const int id = threadIdx.x + blockIdx.x * blockDim.x;
    curandStatePhilox4_32_10_t localState = state[id];
    for (int i = id; i < numBlocks * smemPopNum * 32; i += blockDim.x*gridDim.x) {
        d_mem[i] = curand(&localState);
    }
    state[id] = localState;
}

__device__ 
unsigned int selectParent(unsigned int* smem, unsigned char cidx1, unsigned char cidx2, thread_block_tile<32> tile32) {
    const unsigned int contender1 = smem[32*cidx1 + tile32.thread_rank()];
    const int contender1Score = score(contender1);
    const unsigned int contender2 = smem[32*cidx2 + tile32.thread_rank()];
    const int contender2Score = score(contender2);
    return contender1Score <= contender2Score ? contender1 : contender2;
}

__global__
void testKernel(unsigned int* d_mem, curandStatePhilox4_32_10_t* state) {
    int laneWithIndices = 0;
    bool smemToggle = false; // starts on 0, used for smem flipping and reusing halves of curand4
    bool curandToggle = true;
    unsigned int randIndices;
    uint4 parentSelection;

    extern __shared__ unsigned int smem[];

    thread_block block = this_thread_block();
    thread_block_tile<32> tile32 = tiled_partition<32>(block);

    for (int idx = threadIdx.x; idx < numElemsPerBlock; idx += blockDim.x) {
        smem[idx] = d_mem[idx + blockIdx.x*numElemsPerBlock];
    }

    const int id = threadIdx.x + blockIdx.x * blockDim.x;
    curandStatePhilox4_32_10_t localState = state[id];

    for (int g = 0; g < 1000000; g++) {
        const int parentOffset = smemToggle ? intsPerPop : 0;
        for (int i = 2*tile32.meta_group_rank(); i < smemPopNum; i += 2*tile32.meta_group_size()) {
            laneWithIndices = laneWithIndices % 32;
            if (laneWithIndices == 0) {
                randIndices = curand(&localState);
                randIndices = randIndices & 0x7F7F7F7F;
            }

            unsigned int laneParentInfo = __shfl_sync(mask, randIndices, laneWithIndices);
            const uchar4 splitParentInfo = *reinterpret_cast<uchar4*>(&laneParentInfo);
            laneWithIndices++;

            const unsigned int parent1 = selectParent(smem+parentOffset, splitParentInfo.x, splitParentInfo.y, tile32);
            const unsigned int parent2 = selectParent(smem+parentOffset, splitParentInfo.z, splitParentInfo.w, tile32);

            unsigned int child1Mask;
            unsigned int child2Mask;
            if (curandToggle) {
                parentSelection = curand4(&localState);
                child1Mask = parentSelection.x;
                child2Mask = parentSelection.y;
            } else {
                child1Mask = parentSelection.z;
                child2Mask = parentSelection.w;
            }
            curandToggle = !curandToggle;

            unsigned int child1 = selectMask(parent1, parent2, child1Mask) ^ mutationMask(&localState);
            unsigned int child2 = selectMask(parent1, parent2, child2Mask) ^ mutationMask(&localState);

            const int idx1 = parentOffset + tile32.thread_rank() + i * 32;
            smem[idx1] = child1;
            const int idx2 = parentOffset + tile32.thread_rank() + (i+1) * 32;
            smem[idx2] = child2;
        }
        smemToggle = !smemToggle;
    }

    state[id] = localState;

    for (int idx = threadIdx.x; idx < numElemsPerBlock; idx += blockDim.x) {
        d_mem[idx + blockIdx.x*numElemsPerBlock] = smem[idx];
    }
}

const dim3 dimBlock(numThreads, 1, 1);
const dim3 dimGrid(numBlocks, 1, 1);

int main() {
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

}