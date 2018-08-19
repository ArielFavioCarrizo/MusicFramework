#include "EMFB_STK_COMMON.h"
#include "EMFB_STK_TWOPOLE.h"

#include <Stk.h>
#include <TwoPole.h>

void * emfb_stk_twopole_new(char **exception_desc) {
	return static_cast<void *>(new stk::TwoPole());
}

void emfb_stk_twopole_delete(void *self) {
	delete static_cast<stk::TwoPole *>(self);
}

void emfb_stk_twopole_setGain(void *self, double gain) {
	emfb_stk_setGain<stk::TwoPole>(self, gain);
}

void emfb_stk_twopole_ignoreSampleRateChange(void *self, int ignore) {
	static_cast<stk::TwoPole *>(self)->ignoreSampleRateChange(ignore != 0);
}

void emfb_stk_twopole_setB0(void *self, double b0) {
	static_cast<stk::TwoPole *>(self)->setB0(b0);
}

void emfb_stk_twopole_setA1(void *self, double a1) {
	static_cast<stk::TwoPole *>(self)->setA1(a1);
}

void emfb_stk_twopole_setA2(void *self, double a2) {
	static_cast<stk::TwoPole *>(self)->setA2(a2);
}

void emfb_stk_twopole_setCoefficients(void *self, double b0, double a1, double a2, int clearState) {
	static_cast<stk::TwoPole *>(self)->setCoefficients(b0, a1, a2, clearState != 0);
}

void emfb_stk_twopole_setResonance(void *self, double frequency, double radius, int normalise) {
	static_cast<stk::TwoPole *>(self)->setResonance(frequency, radius, normalise != 0);
}

void emfb_stk_twopole_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::TwoPole>(self, frames, channel);
}

void emfb_stk_twopole_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::TwoPole>(self, iFrames, oFrames, iChannel, oChannel);
}

void emfb_stk_twopole_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel) {
	emfb_stk_tickSubInplace<stk::TwoPole>(self, frames, offset, length, channel);
}

void emfb_stk_twopole_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tickSub<stk::TwoPole>(self, iFrames, oFrames, iOffset, oOffset, length, iChannel, oChannel);
}