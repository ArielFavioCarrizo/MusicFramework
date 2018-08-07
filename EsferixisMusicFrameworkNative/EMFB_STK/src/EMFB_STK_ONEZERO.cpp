#include "EMFB_STK_COMMON.h"
#include "EMFB_STK_ONEZERO.h"

#include <Stk.h>
#include <OneZero.h>

EMFB_STK_API void * emfb_stk_onezero_new(char **exception_desc, double theZero) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::OneZero(theZero));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

EMFB_STK_API void emfb_stk_onezero_delete(void *self) {
	delete static_cast<stk::OneZero *>(self);
}

EMFB_STK_API void emfb_stk_onezero_setB0(void *self, double b0) {
	static_cast<stk::OneZero *>(self)->setB0(b0);
}

EMFB_STK_API void emfb_stk_onezero_setB1(void *self, double b1) {
	static_cast<stk::OneZero *>(self)->setB1(b1);
}

EMFB_STK_API void emfb_stk_onezero_setCoefficients(void *self, double b0, double b1, bool clearState) {
	static_cast<stk::OneZero *>(self)->setCoefficients(b0, b1, clearState);
}

EMFB_STK_API void emfb_stk_onezero_setZero(void *self, double theZero) {
	static_cast<stk::OneZero *>(self)->setZero(theZero);
}

EMFB_STK_API void emfb_stk_onezero_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::OneZero>(self, frames, channel);
}

EMFB_STK_API void emfb_stk_onezero_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::OneZero>(self, iFrames, oFrames, iChannel, oChannel);
}