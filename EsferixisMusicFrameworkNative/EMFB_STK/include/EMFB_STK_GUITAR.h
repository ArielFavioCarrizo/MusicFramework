#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_guitar_new(char **exception_desc, unsigned int nStrings, char *bodyfile);

EMFB_STK_API void emfb_stk_guitar_delete(void *guitar);

EMFB_STK_API void emfb_stk_guitar_clear(void *guitar);

EMFB_STK_API void emfb_stk_guitar_setLoopGain(void *guitar, double gain, int string);

EMFB_STK_API void emfb_stk_guitar_setPluckPosition(void *guitar, double position, int string);

EMFB_STK_API void emfb_stk_guitar_setFrequency(void *guitar, double frequency, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_noteOn(void *guitar, double frequency, double amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_noteOff(void *guitar, double amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_tick(void *guitar, void *iframes, void *oframes, unsigned int iChannel, unsigned int oChannel);