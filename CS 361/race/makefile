STD=--std=c++17 -pthread
GCC=g++
OBJ=obj
BIN=bin

bin/race: src/main.cpp obj/racer.o obj/game_master.o
	[ -d $(BIN) ] || mkdir -p $(BIN)
	${GCC} ${STD} -o bin/race src/main.cpp obj/racer.o obj/game_master.o

obj/racer.o: src/racer.h src/racer.cpp
	[ -d $(OBJ) ] || mkdir -p $(OBJ)
	${GCC} ${STD} -c -o obj/racer.o src/racer.cpp

obj/game_master.o: src/game_master.h src/game_master.cpp
	[ -d $(OBJ) ] || mkdir -p $(OBJ)
	${GCC} ${STD} -c -o obj/game_master.o src/game_master.cpp

run: test0 test1 test2

test0: bin/race
	./bin/race 2
	
test1: bin/race
	./bin/race 4
	
test2: bin/race
	./bin/race 10

.PHONY: doc
doc:
	doxygen config
	cd latex && make

clean:
	rm -f obj/game_master.o
	rm -f obj/racer.o
	rm -f bin/race
	rm -r -f bin
	rm -r -f obj
	rm -r -f html
	rm -r -f latex

