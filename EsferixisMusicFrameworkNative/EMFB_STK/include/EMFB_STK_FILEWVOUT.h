#pragma once

#include "EMFB_STK_COMMON.h"

EMFB_STK_API void * emfb_stk_filewvout_create(char **exception_desc, char *fileName, unsigned int nChannels, unsigned long type, unsigned long format, unsigned int bufferFrames);

EMFB_STK_API void emfb_stk_filewvout_tick(char **exception_desc, void *fileWvOut, void *frames);

EMFB_STK_API void emfb_stk_filewvout_closeFile(char **exception_desc, void *fileWvOut);

EMFB_STK_API void emfb_stk_filewvout_delete(char **exception_desc, void *fileWvOut);
