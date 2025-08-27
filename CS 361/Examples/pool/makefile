
CC=g++
CFLAGS=--std=c++17 -pthread

all: bin/pool bin/seq

bin/pool: obj/searchTask.o src/mainT.cpp obj/threadPool.o obj/move.o obj/board.o
	mkdir -p bin
	${CC} ${CFLAGS} -o bin/pool src/mainT.cpp obj/*.o
bin/seq: obj/searchTask.o src/mainS.cpp obj/threadPool.o obj/move.o obj/board.o
	mkdir -p bin
	${CC} ${CFLAGS} -o bin/seq src/mainS.cpp obj/*.o

obj/move.o: src/move.h src/move.cpp
	mkdir -p obj
	${CC} ${CFLAGS} -c -o obj/move.o src/move.cpp

obj/board.o: src/board.h src/board.cpp
	mkdir -p obj
	${CC} ${CFLAGS} -c -o obj/board.o src/board.cpp

obj/searchTask.o: src/searchTask.h src/task.h src/searchTask.cpp
	mkdir -p obj
	${CC} ${CFLAGS} -c -o obj/searchTask.o src/searchTask.cpp

obj/threadPool.o: src/task.h src/threadPool.h src/threadPool.cpp
	mkdir -p obj
	${CC} ${CFLAGS} -c -o obj/threadPool.o src/threadPool.cpp

res/pool.txt: bin/pool
	mkdir -p res
	./bin/pool > res/pool.txt

res/seq.txt: bin/seq
	mkdir -p res
	./bin/seq > res/seq.txt

run: res/pool.txt res/seq.txt

.PHONY: documentation
documentation: Doxyfile
	doxygen Doxyfile
	cd latex && make

clean:
	rm -f obj/*.o
	rm -f -r obj
	rm -f bin/pool
	rm -f bin/seq
	rm -f -r bin
	rm -f res/pool.txt
	rm -f res/seq.txt
	rm -f -r res
	rm -r -f html
	rm -r -f latex
