//#include "stdio.h"

int printf (const char* format, ...);

void test () {
	int n;
	{
		int a = 5;
		int b = 10;
		n = a * b - 5;
	}

	int a = 0;
	int b = 1;

	printf("Fibbonaci: %d %d", a, b);

	for (int i = 0; i < n-2; i++) {
		int c = a + b;
		printf(" %d", c);
		a = b;
		b = c;
	}

	printf("\n");
}
