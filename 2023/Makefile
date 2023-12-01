CC_FILES = $(wildcard src/*.cpp)
BIN_FILES = $(patsubst src/%.cpp, %, $(CC_FILES))
CC_FLAGS = -Wall -O2 -lm -fomit-frame-pointer -pthread -std=c++0x
CC = g++

all: $(BIN_FILES)

%: src/%.cpp
	clang-format -i $<
	$(CC) $(CC_FLAGS) -o $@ $<

format:
	clang-format -i src/*.cpp

clean:
	rm -f $(BIN_FILES)
