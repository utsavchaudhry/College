STD=--std=c++17 -pthread
GCC=g++
OBJ=obj
BIN=bin

bin/mandelbrot: src/main.cpp obj/bmp.o src/pixel.h src/bmp.h
	[ -d $(BIN) ] || mkdir -p $(BIN)
	${GCC} ${STD} -o bin/mandelbrot src/main.cpp obj/bmp.o

obj/bmp.o: src/bmp.h src/pixel.h src/bmp.cpp
	[ -d $(OBJ) ] || mkdir -p $(OBJ)
	${GCC} ${STD} -c -o obj/bmp.o src/bmp.cpp

mandelbrot: bin/mandelbrot

e0.bmp: bin/mandelbrot
	./bin/mandelbrot -2 1 -1.5 1.5 e0.bmp
	
e1.bmp: bin/mandelbrot
	./bin/mandelbrot -0.5 0 0.3 1.2 e1.bmp

e2.bmp: bin/mandelbrot
	./bin/mandelbrot 0.3 0.4 0.6 0.7 e2.bmp
	
e3.bmp: bin/mandelbrot
	./bin/mandelbrot -0.2 0.0 -1.0 -0.9 e3.bmp
	
e4.bmp: bin/mandelbrot
	./bin/mandelbrot -0.05 -0.01 -1.01 -0.97 e4.bmp

.PHONY: doc
doc:
	doxygen config
	cd latex && make

clean:
	rm -f bin/mandelbrot
	rm -r -f bin
	rm -r -f obj
	rm -r -f html
	rm -r -f latex

