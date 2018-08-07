#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_onepole_new(char **exception_desc, double thePole);

EMFB_STK_API void emfb_stk_onepole_delete(void *self);

EMFB_STK_API void emfb_stk_onepole_setGain(void *self, double gain);

EMFB_STK_API void emfb_stk_onepole_setB0(void *self, double b0);

EMFB_STK_API void emfb_stk_onepole_setA1(void *self, double a1);

EMFB_STK_API void emfb_stk_onepole_setCoefficients(void *self, double b0, double a1, int clearState);

EMFB_STK_API void emfb_stk_onepole_setPole(void *self, double thePole);

EMFB_STK_API void emfb_stk_onepole_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_onepole_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);