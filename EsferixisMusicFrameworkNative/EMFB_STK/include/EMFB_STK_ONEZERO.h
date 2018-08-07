#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_onezero_new(char **exception_desc, double theZero);

EMFB_STK_API void emfb_stk_onezero_delete(void *self);

EMFB_STK_API void emfb_stk_onezero_setB0(void *self, double b0);

EMFB_STK_API void emfb_stk_onezero_setB1(void *self, double b1);

EMFB_STK_API void emfb_stk_onezero_setCoefficients(void *self, double b0, double b1, bool clearState);

EMFB_STK_API void emfb_stk_onezero_setZero(void *self, double theZero);

EMFB_STK_API void emfb_stk_onezero_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_onezero_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);