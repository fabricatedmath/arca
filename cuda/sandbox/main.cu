#include <iostream>

using namespace std;

const int n = 9;

int main() {
    if constexpr (n == 10) {
       cout << "dogs" << endl;
    } else {
      cout << "cats" << endl;
    }
}