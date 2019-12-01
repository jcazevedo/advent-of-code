CC_FILES = $(wildcard src/*.cpp)
BIN_FILES = $(patsubst src/%.cpp, %, $(CC_FILES))
CC_FLAGS = -Wall -O2 -lm -fomit-frame-pointer -pthread -std=c++0x
CC = g++

all: $(BIN_FILES)

%: src/%.cpp
	$(CC) $(CC_FLAGS) -o $@ $<

clean:
	rm -f $(BIN_FILES)
