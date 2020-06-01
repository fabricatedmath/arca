#include <iostream>

using namespace std;

#include "Test.cuh"

int main() {
    cout << "dogs" << endl;
    cout << "cats" << endl;
    Test::call();

    cout << "ulli: " << sizeof(unsigned long long int) << endl;
    cout << "lli: " << sizeof(long long int) << endl;
    cout << "li: " << sizeof(long int) << endl;
    cout << "uli: " << sizeof(unsigned long int) << endl;
}