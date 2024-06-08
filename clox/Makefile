CC = gcc
CFLAGS = -Wall -Wextra -Isrc

SRCDIR = src
TEST_SRCDIR = tests
SRC = $(wildcard $(SRCDIR)/*.c)
TEST_SRC = $(wildcard $(TEST_SRCDIR)/*.c)
TARGET = clox
TEST_TARGET = clox_test

MINUNIT = $(TEST_SRCDIR)/minunit.h
MINUNIT_URL = https://raw.githubusercontent.com/siu/minunit/master/minunit.h

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -o $@ $^

run: $(TARGET)
	./$(TARGET)

$(MINUNIT):
	curl -s -L -o $(MINUNIT) $(MINUNIT_URL)

$(TEST_TARGET): $(MINUNIT) $(SRC) $(TEST_SRC)
	$(CC) $(CFLAGS) -o $(TEST_TARGET) $(TEST_SRC) $(filter-out $(SRCDIR)/main.c, $(SRC))

test: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -f $(TARGET) $(TEST_TARGET) $(MINUNIT)

.PHONY: all run test clean