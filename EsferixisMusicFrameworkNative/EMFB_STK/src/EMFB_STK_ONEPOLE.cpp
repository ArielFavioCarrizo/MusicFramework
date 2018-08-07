#include "EMFB_STK_COMMON.h"
#include "EMFB_STK_ONEPOLE.h"

#include <Stk.h>
#include <OnePole.h>

void * emfb_stk_onepole_new(char **exception_desc, double thePole) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::OnePole(thePole));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_onepole_delete(void *self) {
	delete static_cast<stk::OnePole *>(self);
}

void emfb_stk_onepole_setB0(void *self, double b0) {
	static_cast<stk::OnePole *>(self)->setB0(b0);
}

void emfb_stk_onepole_setA1(void *self, double a1) {
	static_cast<stk::OnePole *>(self)->setA1(a1);
}

void emfb_stk_onepole_setCoefficients(void *self, double b0, double a1, bool clearState) {
	static_cast<stk::OnePole *>(self)->setCoefficients(b0, a1, clearState);
}

void emfb_stk_onepole_setPole(void *self, double thePole) {
	static_cast<stk::OnePole *>(self)->setPole(thePole);
}

void emfb_stk_onepole_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::OnePole>(self, frames, channel);
}

void emfb_stk_onepole_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::OnePole>(self, iFrames, oFrames, iChannel, oChannel);
}