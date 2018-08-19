#include "EMFB_STK_CUBIC.h"
#include "Cubic.h"

void * emfb_stk_cubic_new(char **exception_desc) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::Cubic());
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_cubic_setA1(void *cubic, double a1) {
	static_cast<stk::Cubic *>(cubic)->setA1(a1);
}

void emfb_stk_cubic_setA2(void *cubic, double a2) {
	static_cast<stk::Cubic *>(cubic)->setA2(a2);
}

void emfb_stk_cubic_setA3(void *cubic, double a3) {
	static_cast<stk::Cubic *>(cubic)->setA3(a3);
}

void emfb_stk_cubic_setGain(void *cubic, double gain) {
	emfb_stk_setGain<stk::Cubic>(cubic, gain);
}

void emfb_stk_cubic_setThreshold(void *cubic, double threshold) {
	static_cast<stk::Cubic *>(cubic)->setThreshold(threshold);
}

void emfb_stk_cubic_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::Cubic>(self, frames, channel);
}

void emfb_stk_cubic_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::Cubic>(self, iFrames, oFrames, iChannel, oChannel);
}

void emfb_stk_cubic_tickSubInplace(void *self, void *frames, unsigned int offset, unsigned int length, unsigned int channel) {
	emfb_stk_tickSubInplace<stk::Cubic>(self, frames, offset, length, channel);
}

void emfb_stk_cubic_tickSub(void *self, void *iFrames, void *oFrames, unsigned int iOffset, unsigned int oOffset, unsigned int length, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tickSub<stk::Cubic>(self, iFrames, oFrames, iOffset, oOffset, length, iChannel, oChannel);
}

EMFB_STK_API void emfb_stk_cubic_delete(void *cubic) {
	delete static_cast<stk::Cubic *>(cubic);
}