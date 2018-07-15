#include "EMFB_STK_COMMON.h"

char * emfb_stk_cppStrToCStr(const std::string cppstr) {
	size_t realLength = strlen(cppstr.c_str()) + 1;

	char *cstr = static_cast<char *>(malloc(realLength));

	memcpy(cstr, cppstr.c_str(), realLength);

	return cstr;
}