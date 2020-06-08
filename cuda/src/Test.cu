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

    __global__
    void kernel2() {
        float a25 = 0.84227717;
        float a15 = 0.41796136;
        float a16 = tanhf(a15);
        float a17 = tanhf(a16);
        float a13 = 0.44315594;
        float a14 = rintf(a13);
        float a18 = __fsub_rn(a17, a14);
        float a12 = 0.481494;
        float a19 = __fdividef(a18, a12);
        float a9 = 0.4655599;
        float a10 = fabsf(a9);
        float a8 = 0.50780976;
        float a11 = fmaxf(a10, a8);
        float a20 = __fmul_rz(a19, a11);
        float a21 = erfcinvf(a20);
        float a22 = sinf(a21);
        float a23 = asinf(a22);
        float a7 = 0.3636939;
        float a24 = __fsub_rz(a23, a7);
        float a4 = 0.1781128;
        float a5 = __frcp_rn(a4);
        float a6 = normcdfinvf(a5);
        float a1 = 0.19005537;
        float a2 = __frcp_ru(a1);
        float a3 = exp10f(a2);
        float a26 = norm4df(a25, a24, a6, a3);
        float a27 = erfcxf(a26);
        if (threadIdx.x == 0) {
            printf("\n\n");
            printf("a25 %f\n",a25);
            printf("a15 %f\n",a15);
            printf("a16 %f\n",a16);
            printf("a17 %f\n",a17);
            printf("a13 %f\n",a13);
            printf("a14 %f\n",a14);
            printf("a18 %f\n",a18);
            printf("a12 %f\n",a12);
            printf("a19 %f\n",a19);
            printf("a9 %f\n",a9);
            printf("a10 %f\n",a10);
            printf("a8 %f\n",a8);
            printf("a11 %f\n",a11);
            printf("a20 %f\n",a20);
            printf("a21 %f\n",a21);
            printf("a22 %f\n",a22);
            printf("a23 %f\n",a23);
            printf("a7 %f\n",a7);
            printf("a24 %f\n",a24);
            printf("a4 %f\n",a4);
            printf("a5 %f\n",a5);
            printf("a6 %f\n",a6);
            printf("a1 %f\n",a1);
            printf("a2 %f\n",a2);
            printf("a3 %f\n",a3);
            printf("a26 %f\n",a26);
            printf("a27 %f\n",a27);
            
            
            printf("\n\n");
        }
    }
}

void Test::call() {
    kernel<<<1,32>>>();
    kernel2<<<1,32>>>();
    cudaDeviceSynchronize();
}