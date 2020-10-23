all:
	nvcc main.cu -O2 -gencode=arch=compute_75,code=compute_75 -o a.out
	nvcc forum.cu -O2 -gencode=arch=compute_75,code=compute_75 -o forum

clean:
	rm a.out
