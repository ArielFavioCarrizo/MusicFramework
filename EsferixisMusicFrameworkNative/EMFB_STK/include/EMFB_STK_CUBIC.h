#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_cubic_new(char **exception_desc);

EMFB_STK_API void emfb_stk_cubic_setA1(void *cubic, double a1);

EMFB_STK_API void emfb_stk_cubic_setA2(void *cubic, double a2);

EMFB_STK_API void emfb_stk_cubic_setA3(void *cubic, double a3);

EMFB_STK_API void emfb_stk_cubic_setGain(void *cubic, double gain);

EMFB_STK_API void emfb_stk_cubic_setThreshold(void *cubic, double threshold);

EMFB_STK_API void emfb_stk_cubic_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_cubic_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);

EMFB_STK_API void emfb_stk_cubic_delete(void *cubic);