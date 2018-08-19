#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_twopole_new(char **exception_desc);

EMFB_STK_API void emfb_stk_twopole_delete(void *self);

EMFB_STK_API void emfb_stk_twopole_setGain(void *self, double gain);

EMFB_STK_API void emfb_stk_twopole_ignoreSampleRateChange(void *self, int ignore);

EMFB_STK_API void emfb_stk_twopole_setB0(void *self, double b0);

EMFB_STK_API void emfb_stk_twopole_setA1(void *self, double a1);

EMFB_STK_API void emfb_stk_twopole_setA2(void *self, double a2);

EMFB_STK_API void emfb_stk_twopole_setCoefficients(void *self, double b0, double a1, double a2, int clearState);

EMFB_STK_API void emfb_stk_twopole_setResonance(void *self, double frequency, double radius, int normalise);

EMFB_STK_API void emfb_stk_twopole_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_twopole_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);

EMFB_STK_API void emfb_stk_twopole_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel);

EMFB_STK_API void emfb_stk_twopole_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel);