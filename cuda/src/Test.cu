#include "Test.cuh"

#include <stdio.h>
/*
namespace {
    __global__
    void kernel() {
        for (int i = 0; i < 32; i++) {
            if (threadIdx.x == i) {
                printf("%d\n", threadIdx.x);

                float s;
                float 

                int isFinite = isfinite(0.2);
                printf("%d\n", isFinite);
                printf("%d\n", sizeof(isFinite));
                BFE(1,1,1);
            }
        }
    }
}*/

namespace {
    __global__
    void kernel() {
        if (threadIdx.x == 0) {
            float s;
            float c;
            float angle = 3.1415;
            __sincosf(angle,&s,&c);
            printf("%f:%f\n", s, c);
            printf("%lu:%lu\n", sizeof(long int), sizeof(int));
            int x = sizeof(int);
            printf("%lu\n",sizeof(long long int));
            printf("%lu\n",sizeof(long long));
            printf("%lu\n",sizeof(long));
            printf("%lu\n",sizeof(unsigned long long int));
            printf("%lu\n",sizeof(unsigned));
            printf("%lu\n",sizeof(unsigned int));
            printf("%lu\n",sizeof(unsigned long));
            printf("%lu\n",sizeof(int3));
            printf("%z\n",sizeof(char4));

            int x3 = sizeof(size_t);

            printf("x: %d\n", x3);

            double d = 1.313131;
            d = d * 10000000000000000000000000000;
            printf("%f\n",d);

            unsigned int x2 = __double2uint_ru(d);
            printf("%u\n", x2);

            //printf("%s\n", typeof(a).c_str());
        }
    }
}

void Test::call() {
    kernel<<<1,32>>>();
    cudaDeviceSynchronize();
}