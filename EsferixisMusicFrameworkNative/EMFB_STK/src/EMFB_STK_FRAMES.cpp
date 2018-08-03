#include "EMFB_STK_FRAMES.h"
#include "Stk.h"

#include "Stk.h"

void * emfb_stk_stkframes_new_zero(char **exception_desc, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void * emfb_stk_stkframes_new_valued(char **exception_desc, double value, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(value, nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

unsigned int emfb_stk_stkframes_channels(void *frames) {
	return static_cast<stk::StkFrames *>(frames)->channels();
}

unsigned int emfb_stk_stkframes_nFrames(void *frames) {
	return static_cast<stk::StkFrames *>(frames)->frames();
}

void * emfb_stk_stkframes_clone(char **exception_desc, void *frames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(*static_cast<stk::StkFrames *>(frames)));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_stkframes_addInplace(void *selfFrames, void *otherFrames) {
	(*static_cast<stk::StkFrames *>(selfFrames)) += (*static_cast<stk::StkFrames *>(otherFrames));
}

void emfb_stk_stkframes_mulHomologsInplace(void *selfFrames, void *otherFrames) {
	(*static_cast<stk::StkFrames *>(selfFrames)) *= (*static_cast<stk::StkFrames *>(otherFrames));
}

void emfb_stk_stkframes_delete(void *frames) {
	delete static_cast<stk::StkFrames *>(frames);
}