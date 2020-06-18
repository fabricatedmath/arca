#include "StrLenContainer.cuh"

StrLenContainer::StrLenContainer(char* _strPtr, const int _strLen) 
    : strPtr(_strPtr), strLen(_strLen) {}

char* StrLenContainer::getStrPtr() {
    return strPtr;
}

int StrLenContainer::getStrLen() {
    return strLen;
}