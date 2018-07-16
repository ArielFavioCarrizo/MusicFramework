#include "EMFB_STK_FRAMES.h"
#include "Stk.h"

#include "EMFB_STK_FRAMES.h"

#include "Stk.h"

void * emfb_stk_frames_new_zero(char **exception_desc, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return NULL;
}

void * emfb_stk_frames_new_valued(char **exception_desc, float value, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return NULL;
}

unsigned int emfb_stk_frames_channels(char **exception_desc, void *frames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<stk::StkFrames *>(frames)->channels();
	EMFB_STK_CATCHEXCEPT_END
	return 0;
}

void emfb_stk_frames_delete(char **exception_desc, void *frames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	delete static_cast<stk::StkFrames *>(frames);
	EMFB_STK_CATCHEXCEPT_END
}