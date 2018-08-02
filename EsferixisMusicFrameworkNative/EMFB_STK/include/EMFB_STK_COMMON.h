#pragma once

#ifdef __cplusplus
#include <string>
#include <Stk.h>
#endif

#ifdef _WIN32
	#if EMFB_STK_EXPORTS
		#define EMFB_STK_API extern "C" __declspec(dllexport)
	#else
		#define EMFB_STK_API extern "C" __declspec(dllimport)
	#endif
#else
	#define EMFB_STK extern "C"
#endif

EMFB_STK_API unsigned long emfb_stk_sint16();

#ifdef __cplusplus

char * emfb_stk_cppStrToCStr(const std::string cppstr);

#define EMFB_STK_CATCHEXCEPT_BEGIN \
	*exception_desc = NULL; \
	try {
#define EMFB_STK_CATCHEXCEPT_END \
	} \
	catch (stk::StkError e) { \
		*exception_desc = emfb_stk_cppStrToCStr(e.getMessage()); \
	}
#endif