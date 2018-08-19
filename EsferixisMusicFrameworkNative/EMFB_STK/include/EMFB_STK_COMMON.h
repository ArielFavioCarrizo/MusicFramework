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

#ifdef __cplusplus


template<typename StkUnitType>
void emfb_stk_tickInplace(void *self, void *frames, unsigned int channel) {
	static_cast<StkUnitType *>(self)->tick(*static_cast<stk::StkFrames *>(frames), channel);
}

template<typename StkUnitType>
void emfb_stk_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	static_cast<StkUnitType *>(self)->tick(*static_cast<stk::StkFrames *>(iFrames), *static_cast<stk::StkFrames *>(oFrames), iChannel, oChannel);
}

template<typename StkUnitType>
void emfb_stk_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel) {
	StkUnitType& self_casted = *static_cast<StkUnitType *>(self);
	stk::StkFrames& frames_casted = *static_cast<stk::StkFrames *>(frames);

	size_t hop = (size_t)frames_casted.channels();

	stk::StkFloat *data = &frames_casted[hop * (size_t)offset + (size_t)channel];

	for (size_t i = 0; i<length; i++) {
		*data = self_casted.tick(*data);
		data += hop;
	}
}

template<typename StkUnitType>
void emfb_stk_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel) {
	StkUnitType& self_casted = *static_cast<StkUnitType *>(self);
	stk::StkFrames& iFrames_casted = *static_cast<stk::StkFrames *>(iFrames);
	stk::StkFrames& oFrames_casted = *static_cast<stk::StkFrames *>(oFrames);

	size_t iHop = (size_t)iFrames_casted.channels();
	size_t oHop = (size_t)oFrames_casted.channels();

	stk::StkFloat *srcData = &iFrames_casted[iHop * (size_t)iOffset + (size_t)iChannel];
	stk::StkFloat *dstData = &oFrames_casted[oHop * (size_t)oOffset + (size_t)oChannel];

	for (size_t i = 0; i<length; i++) {
		*dstData = self_casted.tick(*srcData);
		srcData += iHop;
		dstData += oHop;
	}
}

template<typename StkUnitType>
void emfb_stk_setGain(void *self, double gain) {
	static_cast<StkUnitType *>(self)->setGain(gain);
}

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