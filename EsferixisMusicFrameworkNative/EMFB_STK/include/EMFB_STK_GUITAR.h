#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_guitar_new(char **exception_desc, unsigned int nStrings, char *bodyfile);

EMFB_STK_API void emfb_stk_guitar_delete(char **exception_desc, void *guitar);

EMFB_STK_API void emfb_stk_guitar_clear(char **exception_desc, void *guitar);

EMFB_STK_API void emfb_stk_guitar_setLoopGain(char **exception_desc, void *guitar, double gain, int string);

EMFB_STK_API void emfb_stk_guitar_setPluckPosition(char **exception_desc, void *guitar, double position, int string);

EMFB_STK_API void emfb_stk_guitar_setFrequency(char **exception_desc, void *guitar, double frequency, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_noteOn(char **exception_desc, void *guitar, double frequency, double amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_noteOff(char **exception_desc, void *guitar, double amplitude, unsigned int string);

EMFB_STK_API void emfb_stk_guitar_tick(char **exception_desc, void *guitar, void *iframes, void *oframes, unsigned int iChannel, unsigned int oChannel);