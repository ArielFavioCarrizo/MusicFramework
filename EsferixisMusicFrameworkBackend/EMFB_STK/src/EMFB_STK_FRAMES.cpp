#include "EMFB_STK_FRAMES.h"
#include "Stk.h"

#include "EMFB_STK_FRAMES.h"

#include "Stk.h"

void * emfb_stk_frames_new_zero(unsigned int nFrames, unsigned int nChannels) {
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
}

void * emfb_stk_frames_new_valued(float value, unsigned int nFrames, unsigned int nChannels) {
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
}

unsigned int emfb_stk_frames_channels(void *frames) {
	return static_cast<stk::StkFrames *>(frames)->channels();
}

void emfb_stk_frames_delete(void *frames) {
	return delete static_cast<stk::StkFrames *>(frames);
}