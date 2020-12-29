EXECUTABLES = input-gen

CFLAGS = -lm -O3 -fopenmp -Wall

all: $(EXECUTABLES)

input-gen: input-gen.c ../utils/utils.c
	gcc $(CFLAGS) -std=gnu99 -o$@ $^ -lm

clean:
	rm -f $(EXECUTABLES) *.o
