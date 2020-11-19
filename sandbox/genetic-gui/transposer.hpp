#pragma once

#include <thread>
#include <boost/lockfree/queue.hpp>

template <class T>
class Message {
public:
    const int row;
    const T val;
    Message(const int _row, const T _val)
        : row(_row), val(_val) {}
    
};

template <class T>
class Transposer {
private:
    const int queueSize = 100;
    const int numBuckets;
    const int numEntries;
    volatile bool shouldStop = false;
    volatile T* ys;
    volatile int* counts;
    void producer();
    void consumer();
    std::thread* producerThread;
    std::thread* consumerThread;
    boost::lockfree::queue<Message<T>*>* queue;
    
public:
    Transposer(int _numBuckets, int _numEntries);
    void start();
    void plotRow(const char* name, const int row);
    void transpose();
    ~Transposer();
};

template class Transposer<float>;
template class Transposer<int>;