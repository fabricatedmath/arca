#pragma once

class PtxCompiler {
private:
    char* ptxStr;
    size_t ptxStrLen;
    char* logStr;
    size_t logStrLen;
public:
    PtxCompiler();
    int compile(const char* str, const int strLen, bool rdc=false);

    char* getPtxStr();
    size_t getPtxStrLen();

    char* getLogStr();
    size_t getLogStrLen();
    
    ~PtxCompiler();
};