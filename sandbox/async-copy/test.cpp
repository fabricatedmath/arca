#include <iostream>
#include <thread>
#include <boost/lockfree/queue.hpp>
#include <boost/chrono.hpp>

using namespace std;

class Dog {
private:
    const int dog;
public:
    Dog(int _dog) : dog(_dog) {}
    int getDog() { return dog; }
};

boost::lockfree::queue<Dog*> queue(100);

void foo() 
{
    for (int i = 0; i < 20; i++) {
        queue.push(new Dog(i));
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
}

void bar(int x)
{
    int c = 0;
    while(true) {
        Dog* d;
        if (queue.pop(d)) {
            c++;
            cout << d->getDog() << endl;
            delete d;
        }
    }
}

int main() {

    
    cout << "dogs" << endl;

    thread first (foo);
    thread second (bar, 0);

    first.join();
    second.join();

    cout << "done" << endl;
}