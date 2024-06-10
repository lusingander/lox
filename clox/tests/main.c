#include "minunit.h"

#include "../src/chunk.h"
#include "../src/scanner.h"

MU_TEST(test_chunk) {
    Chunk chunk;

    initChunk(&chunk);
	mu_check(chunk.count == 0);
	mu_check(chunk.capacity == 0);

    writeChunk(&chunk, 2, 10);
	mu_check(chunk.count == 1);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 3, 11);
	mu_check(chunk.count == 2);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 0, 20);
    writeChunk(&chunk, 0, 20);
    writeChunk(&chunk, 1, 20);
    writeChunk(&chunk, 1, 20);
    writeChunk(&chunk, 2, 20);
    writeChunk(&chunk, 2, 20);
	mu_check(chunk.count == 8);
	mu_check(chunk.capacity == 8);

    writeChunk(&chunk, 3, 30);
	mu_check(chunk.count == 9);
	mu_check(chunk.capacity == 16);

    freeChunk(&chunk);
	mu_check(chunk.count == 0);
	mu_check(chunk.capacity == 0);
}

MU_TEST(test_scanner) {
	const char* source =
		"(} =;==+	<=>\n"
		"*\r1.23// +-\n"
		"/+/\"+-\"\n"
		"x123 an and classes f fu fun\n";
	initScanner(source);
	mu_check(scanToken().type == TOKEN_LEFT_PAREN);
	mu_check(scanToken().type == TOKEN_RIGHT_BRACE);
	mu_check(scanToken().type == TOKEN_EQUAL);
	mu_check(scanToken().type == TOKEN_SEMICOLON);
	mu_check(scanToken().type == TOKEN_EQUAL_EQUAL);
	mu_check(scanToken().type == TOKEN_PLUS);
	mu_check(scanToken().type == TOKEN_LESS_EQUAL);
	mu_check(scanToken().type == TOKEN_GREATER);

	mu_check(scanToken().type == TOKEN_STAR);
	mu_check(scanToken().type == TOKEN_NUMBER);

	mu_check(scanToken().type == TOKEN_SLASH);
	mu_check(scanToken().type == TOKEN_PLUS);
	mu_check(scanToken().type == TOKEN_SLASH);
	mu_check(scanToken().type == TOKEN_STRING);

	mu_check(scanToken().type == TOKEN_IDENTIFIER);
	mu_check(scanToken().type == TOKEN_IDENTIFIER);
	mu_check(scanToken().type == TOKEN_AND);
	mu_check(scanToken().type == TOKEN_IDENTIFIER);
	mu_check(scanToken().type == TOKEN_IDENTIFIER);
	mu_check(scanToken().type == TOKEN_IDENTIFIER);
	mu_check(scanToken().type == TOKEN_FUN);

	// printf("%d", scanToken().type);
}

MU_TEST_SUITE(test_suite) {
	MU_RUN_TEST(test_chunk);
	MU_RUN_TEST(test_scanner);
}

int main() {
	MU_RUN_SUITE(test_suite);
	MU_REPORT();
	return MU_EXIT_CODE;
}