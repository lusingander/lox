#include "minunit.h"

#include "../src/add.h"
#include "../src/chunk.h"

MU_TEST(test_add) {
	mu_check(add(1, 2) == 3);
}

MU_TEST(test_chunk) {
    Chunk chunk;

    initChunk(&chunk);
	mu_check(chunk.count == 0);
	mu_check(chunk.capacity == 0);

    writeChunk(&chunk, 2);
	mu_check(chunk.count == 1);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 3);
	mu_check(chunk.count == 2);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 0);
    writeChunk(&chunk, 0);
    writeChunk(&chunk, 1);
    writeChunk(&chunk, 1);
    writeChunk(&chunk, 2);
    writeChunk(&chunk, 2);
	mu_check(chunk.count == 8);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 3);
	mu_check(chunk.count == 9);
	mu_check(chunk.capacity == 16);

    freeChunk(&chunk);
	mu_check(chunk.count == 0);
	mu_check(chunk.capacity == 0);
}

MU_TEST_SUITE(test_suite) {
	MU_RUN_TEST(test_add);
	MU_RUN_TEST(test_chunk);
}

int main() {
	MU_RUN_SUITE(test_suite);
	MU_REPORT();
	return MU_EXIT_CODE;
}