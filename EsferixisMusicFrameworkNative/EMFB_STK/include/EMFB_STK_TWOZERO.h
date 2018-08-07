#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_twozero_new(char **exception_desc);

EMFB_STK_API void emfb_stk_twozero_delete(void *self);

EMFB_STK_API void emfb_stk_twozero_setGain(void *self, double gain);

EMFB_STK_API void emfb_stk_twozero_ignoreSampleRateChange(void *self, int ignore);

EMFB_STK_API void emfb_stk_twozero_setB0(void *self, double b0);

EMFB_STK_API void emfb_stk_twozero_setB1(void *self, double b1);

EMFB_STK_API void emfb_stk_twozero_setB2(void *self, double b2);

EMFB_STK_API void emfb_stk_twozero_setCoefficients(void *self, double b0, double b1, double b2, int clearState);

EMFB_STK_API void emfb_stk_twozero_setNotch(void *self, double frequency, double radius);

EMFB_STK_API void emfb_stk_twozero_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_twozero_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);