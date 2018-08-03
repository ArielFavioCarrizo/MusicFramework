#include "EMFB_STK_COMMON.h"

#include <Stk.h>

unsigned long emfb_stk_sint16() {
	return stk::Stk::STK_SINT16;
}

double emfb_stk_sampleRate() {
	return stk::Stk::sampleRate();
}

void emfb_stk_setSampleRate(double rate) {
	stk::Stk::setSampleRate(rate);
}

char * emfb_stk_cppStrToCStr(const std::string cppstr) {
	size_t realLength = strlen(cppstr.c_str()) + sizeof(char);

	char *cstr = static_cast<char *>(malloc(realLength));

	memcpy(cstr, cppstr.c_str(), realLength);

	return cstr;
}