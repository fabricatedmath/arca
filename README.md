# arca
In progress...

nvrtc mutation framework for sm granularity kernels

## Installation
### C++ Lib
```bash
cd $CLONE_DIR
cd cuda
mkdir build
cd build
cmake ..
make
```

### Haskell
```bash
cd $CLONE_DIR
cd arca/haskell
stack install
```

This creates a bunch of executables in `$HOME/.local/bin`

To run these executables add the following to your `.bashrc` file:
```bash
export LD_LIBRARY_PATH=$CLONE_DIR/cuda/build:$LD_LIBRARY_PATH
```
