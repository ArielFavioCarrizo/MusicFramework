#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_frames_new_zero(unsigned int nFrames, unsigned int nChannels);

EMFB_STK_API void * emfb_stk_frames_new_valued(float value, unsigned int nFrames, unsigned int nChannels);

EMFB_STK_API unsigned int emfb_stk_frames_channels(void *frames);

EMFB_STK_API void emfb_stk_frames_delete(void *frames);