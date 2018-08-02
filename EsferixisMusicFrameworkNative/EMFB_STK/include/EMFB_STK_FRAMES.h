#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_stkframes_new_zero(char **exception_desc, unsigned int nFrames, unsigned int nChannels);

EMFB_STK_API void * emfb_stk_stkframes_new_valued(char **exception_desc, float value, unsigned int nFrames, unsigned int nChannels);

EMFB_STK_API unsigned int emfb_stk_stkframes_channels(char **exception_desc, void *frames);

EMFB_STK_API void emfb_stk_stkframes_delete(char **exception_desc, void *frames);