#pragma once

#ifdef __cplusplus
#include <string>
#include <Stk.h>
#include <stdexcept>
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

// Provisto para evitar problemas con las diferentes bibliotecas del runtime de C
EMFB_STK_API void emfb_stk_cfree(void *ptr);

EMFB_STK_API unsigned long emfb_stk_sint16();
EMFB_STK_API double emfb_stk_sampleRate();
EMFB_STK_API void emfb_stk_setSampleRate(double rate);

template<typename StkUnitType>
void emfb_stk_tickInplace(void *self, void *frames, unsigned int channel) {
	static_cast<StkUnitType *>(self)->tick(*static_cast<stk::StkFrames *>(frames), channel);
}

template<typename StkUnitType>
void emfb_stk_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	static_cast<StkUnitType *>(self)->tick(*static_cast<stk::StkFrames *>(iFrames), *static_cast<stk::StkFrames *>(oFrames), iChannel, oChannel);
}

#ifdef __cplusplus

char * emfb_stk_cppStrToCStr(const std::string cppstr);

#define EMFB_STK_CATCHEXCEPT_BEGIN \
	*exception_desc = NULL; \
	try {
#define EMFB_STK_CATCHEXCEPT_END \
	} \
	catch (stk::StkError e) { \
		*exception_desc = emfb_stk_cppStrToCStr(e.getMessage()); \
	} \
	catch (const std::exception& e) { \
		*exception_desc = emfb_stk_cppStrToCStr( std::string( e.what() ) ); \
	}
#endif