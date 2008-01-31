#include <stdio.h>

void main() {
	FILE *f = fopen("rr", "w");
	char *buf = "a";
	fseek(f, 10000, SEEK_SET);
	fwrite(buf, 1, 1, f);
	fclose(f);
}