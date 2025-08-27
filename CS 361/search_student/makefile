STD=--std=c++17 -pthread
GCC=g++
OBJ=obj
BIN=bin

all: bin/search run

bin/search: src/main.cpp obj/experiments.o obj/find_target.o
	[ -d $(BIN) ] || mkdir -p $(BIN)
	${GCC} ${STD} -o bin/search src/main.cpp obj/experiments.o obj/find_target.o

obj/experiments.o: src/experiments.h src/experiments.cpp
	[ -d $(OBJ) ] || mkdir -p $(OBJ)
	${GCC} ${STD} -c -o obj/experiments.o src/experiments.cpp
 
obj/find_target.o: src/find_target.h src/find_target.cpp
	[ -d $(OBJ) ] || mkdir -p $(OBJ)
	${GCC} ${STD} -c -o obj/find_target.o src/find_target.cpp

run: test0 test1 test2 test3 test4 test5

test0: bin/search
	./bin/search 0 || true
test1: bin/search
	./bin/search 1 || true
test2: bin/search
	./bin/search 2 || true
test3: bin/search
	./bin/search 3 || true
test4: bin/search
	./bin/search 4 || true
test5: bin/search
	./bin/search 5 || true

.PHONY: doc
doc:
	doxygen config
	cd latex && make

clean:
	rm -f obj/find_target.o
	rm -f obj/experiments.o
	rm -f bin/search
	rm -r -f bin
	rm -r -f obj
	rm -r -f html
	rm -r -f latex
