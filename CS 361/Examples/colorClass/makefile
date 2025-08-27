
CC=g++
CFLAGS=--std=c++17 -pthread

all: bin/bmp examples/e1.bmp examples/e2.bmp

bin/bmp: src/main.cpp obj/bmp.o src/pixel.h src/bmp.h
	${CC} ${CFLAGS} -o bin/bmp src/main.cpp obj/bmp.o

obj/bmp.o: src/bmp.h src/pixel.h src/bmp.cpp
	${CC} ${CFLAGS} -c -o obj/bmp.o src/bmp.cpp

examples/e1.bmp: bin/bmp
	./bin/bmp 20 100 examples/e1.bmp

examples/e2.bmp: bin/bmp
	./bin/bmp 200 200 examples/e2.bmp

.PHONEY: documentation
documentation: config
	doxygen config
	cd latex && make

clean:
	rm -f bin/bmp
	rm -f ob/bmp.o
	rm -f examples/*.bmp
	rm -r -f html
	rm -r -f latex


