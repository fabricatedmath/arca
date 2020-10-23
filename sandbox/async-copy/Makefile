all:
	nvcc -O2 -lineinfo -gencode arch=compute_75,code=sm_75 --ptxas-options=-v *.cu 
clean:
	rm a.out
