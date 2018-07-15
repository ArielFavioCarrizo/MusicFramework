#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_filewvout_create();

EMFB_STK_API void emfb_stk_filewvout_delete(void *fileWvOut);

EMFB_STK_API void emfb_stk_filewvout_openFile(void *fileWvOut, char **exception_desc, char *fileName, unsigned int nChannels, unsigned long type, unsigned long format);

EMFB_STK_API void emfb_stk_filewvout_tick(void *fileWvOut, void *frames);