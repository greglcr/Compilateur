CC = gcc
CFLAGS = -I bdwgc/include -Wall -Wextra -pedantic -O2 -DPRT_USE_GC

objects = alloc.o log.o show.o string.o div.o main.o

all: libprt.a

libprt.a: $(objects) gc.o
	$(AR) rcs libprt.a $@ $^

$(objects): %.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

gc.o: bdwgc/extra/gc.c
	$(CC) -o gc.o bdwgc/extra/gc.c -c $(CFLAGS)

clean:
	rm -f *.o
	rm -f *.a

.PHONY: all clean
