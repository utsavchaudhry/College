
bin/blue: src/blue.cpp
	[ -d bin ] || mkdir -p bin
	g++ --std=c++17 -pthread -obin/blue src/blue.cpp
run: blue.bmp

blue.bmp: bin/blue
	./bin/blue

.PHONEY: documentation
documentation:
	doxygen config
	cd latex && make

clean:
	rm -f bin/blue
	rm -f -d bin
	rm -f blue.bmp
	rm -f -r html
	rm -f -r latex

