#include "filehl.h"

FILE* hl_fopen_r(const char* path) {
	return fopen(path, "r");
}

FILE* hl_fopen_w(const char* path) {
	return fopen(path, "w");
}

usz hl_fsize(FILE* stream) {
	usz pos = ftell(stream);
	fseek(stream, 0, SEEK_END);
	usz size = ftell(stream);
	if (pos != size)
		fseek(stream, pos, SEEK_SET);
	return size;
}

usz hl_fread(FILE* stream, void* data, usz len) {
	return fread(data, 1, len, stream);
}

usz hl_fwrite(FILE* stream, const void* data, usz len) {
	return fwrite(data, 1, len, stream);
}
