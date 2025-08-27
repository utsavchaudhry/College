
bin/colors: src/colorThreads.cpp
	[ -d bin ] || mkdir -p bin
	g++ --std=c++17 -pthread -o bin/colors src/colorThreads.cpp

example.bmp: bin/colors
	./bin/colors

run: example.bmp

.PHONEY: documentation
documentation: config
	doxygen config
	cd latex && make

clean:
	rm -f bin/colors
	rm -f -d bin
	rm -f example.bmp
	rm -f -r html
	rm -f -r latex

