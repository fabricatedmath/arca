#include "test2.cuh"

#include <iostream>

__device__
void test2() {
    int x = 30;
    x = __clz(x);
    printf("dogs%d\n", x);
}