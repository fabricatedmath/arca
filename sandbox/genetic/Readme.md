# Cuda Genetic example

Simple Cuda Genetic example stored exclusively in shared memory for speed. Was curious to see how fast generations could be using shared memory only, might allow cross SM sharing of populations later.

Due to limitations of 2080ti, can store 128 individuals in shared memory. Each individual is 1024 bits long and is scored on the simple task of matching the bits of a target 1024 bit number. Currently doing roughly 1,000,000 generations per second per SM. Given 2080ti has 68 SMs, can do 68 million generations per second. 
