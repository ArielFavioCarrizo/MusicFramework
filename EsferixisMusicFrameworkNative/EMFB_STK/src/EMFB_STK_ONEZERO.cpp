#include "EMFB_STK_COMMON.h"
#include "EMFB_STK_ONEZERO.h"

#include <Stk.h>
#include <OneZero.h>

void * emfb_stk_onezero_new(char **exception_desc, double theZero) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::OneZero(theZero));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_onezero_delete(void *self) {
	delete static_cast<stk::OneZero *>(self);
}

void emfb_stk_onezero_setGain(void *self, double gain) {
	emfb_stk_setGain<stk::OneZero>(self, gain);
}

void emfb_stk_onezero_setB0(void *self, double b0) {
	static_cast<stk::OneZero *>(self)->setB0(b0);
}

void emfb_stk_onezero_setB1(void *self, double b1) {
	static_cast<stk::OneZero *>(self)->setB1(b1);
}

void emfb_stk_onezero_setCoefficients(void *self, double b0, double b1, int clearState) {
	static_cast<stk::OneZero *>(self)->setCoefficients(b0, b1, clearState != 0);
}

void emfb_stk_onezero_setZero(void *self, double theZero) {
	static_cast<stk::OneZero *>(self)->setZero(theZero);
}

void emfb_stk_onezero_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::OneZero>(self, frames, channel);
}

void emfb_stk_onezero_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::OneZero>(self, iFrames, oFrames, iChannel, oChannel);
}

void emfb_stk_onezero_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel) {
	emfb_stk_tickSubInplace<stk::OneZero>(self, frames, offset, length, channel);
}

void emfb_stk_onezero_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tickSub<stk::OneZero>(self, iFrames, oFrames, iOffset, oOffset, length, iChannel, oChannel);
}