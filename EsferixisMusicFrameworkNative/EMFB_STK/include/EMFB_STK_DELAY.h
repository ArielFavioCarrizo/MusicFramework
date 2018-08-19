#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_delay_new(char **exception_desc, unsigned long delay, unsigned long maxDelay);

EMFB_STK_API void emfb_stk_delay_delete(void *self);

EMFB_STK_API void emfb_stk_delay_setGain(void *self, double gain);

EMFB_STK_API unsigned long emfb_stk_delay_getMaximumDelay(void *self);

EMFB_STK_API void emfb_stk_delay_setMaximumDelay(void *self, unsigned long delay);

EMFB_STK_API void emfb_stk_delay_setDelay(char **exception_desc, void *self, unsigned long delay);

EMFB_STK_API unsigned long emfb_stk_delay_getDelay(void *self);

EMFB_STK_API void emfb_stk_delay_tickInplace(void *self, void *frames, unsigned int channel);

EMFB_STK_API void emfb_stk_delay_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel);

EMFB_STK_API void emfb_stk_delay_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel);

EMFB_STK_API void emfb_stk_delay_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel);