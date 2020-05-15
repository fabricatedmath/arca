#include <iostream>

extern "C" {
    void test(char* c) {
        printf("string i got is: %s\n", c);
    }
}