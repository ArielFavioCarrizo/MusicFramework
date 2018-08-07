#include "EMFB_STK_COMMON.h"
#include "EMFB_STK_TWOZERO.h"

#include <Stk.h>
#include <TwoZero.h>

void * emfb_stk_twozero_new(char **exception_desc) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::TwoZero());
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_twozero_delete(void *self) {
	delete static_cast<stk::TwoZero *>(self);
}

void emfb_stk_twozero_setGain(void *self, double gain) {
	emfb_stk_setGain<stk::TwoZero>(self, gain);
}

void emfb_stk_twozero_ignoreSampleRateChange(void *self, int ignore) {
	static_cast<stk::TwoZero *>(self)->ignoreSampleRateChange(ignore != 0);
}

void emfb_stk_twozero_setB0(void *self, double b0) {
	static_cast<stk::TwoZero *>(self)->setB0(b0);
}

void emfb_stk_twozero_setB1(void *self, double b1) {
	static_cast<stk::TwoZero *>(self)->setB1(b1);
}

void emfb_stk_twozero_setB2(void *self, double b2) {
	static_cast<stk::TwoZero *>(self)->setB2(b2);
}

void emfb_stk_twozero_setCoefficients(void *self, double b0, double b1, double b2, int clearState) {
	static_cast<stk::TwoZero *>(self)->setCoefficients(b0, b1, b2, clearState != 0);
}

void emfb_stk_twozero_setNotch(void *self, double frequency, double radius) {
	static_cast<stk::TwoZero *>(self)->setNotch(frequency, radius);
}

void emfb_stk_twozero_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::TwoZero>(self, frames, channel);
}

void emfb_stk_twozero_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::TwoZero>(self, iFrames, oFrames, iChannel, oChannel);
}