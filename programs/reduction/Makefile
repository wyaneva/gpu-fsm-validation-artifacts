EXECUTABLES = reduce transitionpairs

CFLAGS = -lm -O3 -Wall

UTILS := ../utils/utils.c

all: $(EXECUTABLES)

reduce: reduce.c $(UTILS)
	clang $(CFLAGS) -std=gnu99 -o$@ $^

transitionpairs: transitionpairs.c $(UTILS)
	clang $(CFLAGS) -std=gnu99 -o$@ $^

clean:
	rm -f $(EXECUTABLES) *.o
