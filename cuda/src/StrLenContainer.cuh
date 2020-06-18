#pragma once

class StrLenContainer {
private:
    char* strPtr;
    const int strLen;
public:
    StrLenContainer(char* _strPtr, const int strLn);
    char* getStrPtr();
    int getStrLen();
};