#include "EMFB_STK_FILEWVOUT.h"

#include <Stk.h>
#include <FileWvOut.h>
#include <string>

void * emfb_stk_filewvout_create(char **exception_desc, void *fileWvOut, char *fileName, unsigned int nChannels, unsigned long type, unsigned long format, unsigned int bufferFrames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::FileWvOut(std::string(fileName), nChannels, type, format, bufferFrames));
	EMFB_STK_CATCHEXCEPT_END
	return NULL;
}

void emfb_stk_filewvout_delete(char **exception_desc, void *fileWvOut) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	delete static_cast<stk::FileWvOut *>(fileWvOut);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_filewvout_tick(char **exception_desc, void *fileWvOut, void *frames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::FileWvOut *>(fileWvOut)->tick(*((static_cast<stk::StkFrames *>(frames))));
	EMFB_STK_CATCHEXCEPT_END
}