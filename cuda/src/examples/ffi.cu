#include "ffi.cuh"

#include <iostream>

using namespace std;

extern "C" {
    void test(char* str, int len) {
        cout << "called" << endl;
        cout << str << endl;
        cout << len << endl;
    }
}