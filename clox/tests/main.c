#include "minunit.h"
#include "../src/add.h"

MU_TEST(test_add) {
	mu_check(add(1, 2) == 3);
}

MU_TEST_SUITE(test_suite) {
	MU_RUN_TEST(test_add);
}

int main() {
	MU_RUN_SUITE(test_suite);
	MU_REPORT();
	return MU_EXIT_CODE;
}