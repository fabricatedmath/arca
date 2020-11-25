#include "transposer.hpp"

#include <iostream>
#include <thread>

#include <math.h>

#include <implot.h>

using namespace std;

#define PI 3.14159265

template<class T>
Transposer<T>::Transposer(int _numBuckets, int _numEntries) 
    : numBuckets(_numBuckets), numEntries(_numEntries) {
    ys = new T[numEntries * numBuckets];
    counts = new int[numBuckets];
    for (int i = 0; i < numBuckets; i++) {
        counts[i] = 0;
    }
    queue = new boost::lockfree::queue<Message<T>*>(queueSize);
}

template<>
void Transposer<int>::producer() {
    int c = 0;
    while(!shouldStop) {
        for (int i = 0; i < numBuckets; i++) {
            queue->push(new Message<int>(i,c+i));
        }
        c++;
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
}

template<>
void Transposer<float>::producer() {
    int c = 0;
    while(!shouldStop) {
        for (int i = 0; i < numBuckets; i++) {
            float v = c+i;
            v = sin(v * PI / 10);
            queue->push(new Message<float>(i,v));
        }
        c++;
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
}


template<class T>
void Transposer<T>::consumer() {
    Message<T>* message;
    while(!shouldStop) {
        if(queue->pop(message)) {
            const int row = message->row;
            const T val = message->val;
            int currentCount = counts[row];
            ys[row*numEntries+currentCount] = val;
            counts[row] = currentCount+1;
            delete message;
        }
    }
    while(queue->pop(message)) {
        delete message;
    }
}

template<class T>
void Transposer<T>::start() {
    producerThread = new thread(&Transposer::producer, this);
    consumerThread = new thread(&Transposer::consumer, this);
}

template<class T>
void Transposer<T>::plotRow(const char* name, const int row) {
    T* data = (T*)ys+row*numEntries;
    ImPlot::PlotLine(name, data, counts[row]);
}

template<class T>
Transposer<T>::~Transposer() {
    shouldStop = true;
    producerThread->join();
    consumerThread->join();
    delete producerThread;
    delete consumerThread;
    delete queue;
    delete [] ys;
    delete [] counts;
}