#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_guitar_new();

EMFB_STK_API void emfb_stk_guitar_delete(void *guitar);

EMFB_STK_API void emfb_stk_guitar_clear(void *guitar);

EMFB_STK_API void emfb_stk_guitar_setLoopGain(void *guitar, float gain, int string);

EMFB_STK_API void emfb_stk_guitar_setPluckPosition(void *guitar, float position, int string);

EMFB_STK_API void emfb_stk_guitar_noteOn(void *guitar, float frequency, float amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_noteOff(void *guitar, float amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_tick(void *guitar, void *iframes, void *oframes, unsigned int iChannel, unsigned int oChannel);