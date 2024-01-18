CC = gcc
CFLAGS = -Wall -Wextra -pedantic -O2

objects = alloc.o log.o show.o strconcat.o div.o main.o

all: libprt.a

libprt.a: $(objects)
	$(AR) rcs libprt.a $@ $^

$(objects): %.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	rm -f *.o
	rm -f *.a

.PHONY: all clean
