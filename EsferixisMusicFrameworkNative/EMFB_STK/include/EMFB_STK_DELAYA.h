#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_delaya_new(char **exception_desc, unsigned long delay, unsigned long maxDelay);

EMFB_STK_API void emfb_stk_delaya_delete(void *self);

EMFB_STK_API void emfb_stk_delaya_clear(void *self);

EMFB_STK_API unsigned long emfb_stk_delaya_getMaximumDelay(void *self);

EMFB_STK_API void emfb_stk_delaya_setMaximumDelay(void *self, unsigned long delay);

EMFB_STK_API void emfb_stk_delaya_setDelay(char **exception_desc, void *self, unsigned long delay);

EMFB_STK_API unsigned long emfb_stk_delaya_getDelay(void *self);

EMFB_STK_API void emfb_stk_delaya_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_delaya_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);