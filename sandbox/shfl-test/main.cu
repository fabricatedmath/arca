#include <iostream>

using namespace std;

__global__
void shfl_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __shfl_sync(0xFFFFFFFF, tid, tid-10);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}

__global__
void shfl_up_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __shfl_up_sync(0xFFFFFFFF, tid, 2);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}

__global__
void shfl_down_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __shfl_down_sync(0xFFFFFFFF, tid, 10);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}

__global__
void ballot_sync_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __ballot_sync(0xFFFFFFFF, tid % 2 == 0);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}

__global__
void any_sync_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __any_sync(0xFFFFFFFF, tid % 2 == 0);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}

/* can only do this with compute 8.0
__global__
void reduce_add_kernel() {
    int tid = threadIdx.x;
    int otherThreadIdx = __reduce_add_sync(0x1F, tid);
    for (int i = 0; i < 32; i++) {
        if (i == tid) {
            printf("My name is %d and i have %d\n", tid, otherThreadIdx);
        }
    }
}
*/

int main() {
    cout << "shfl_kernel" << endl;
    shfl_kernel<<<1,32>>>();
    cudaDeviceSynchronize();

    cout << "shfl_up_kernel" << endl;
    shfl_up_kernel<<<1,32>>>();
    cudaDeviceSynchronize();

    cout << "shfl_down_kernel" << endl;
    shfl_down_kernel<<<1,32>>>();
    cudaDeviceSynchronize();

    cout << "ballot_sync_kernel" << endl;
    ballot_sync_kernel<<<1,32>>>();
    cudaDeviceSynchronize();

    cout << "any_sync_kernel" << endl;
    any_sync_kernel<<<1,32>>>();
    cudaDeviceSynchronize();
}