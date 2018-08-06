#include "EMFB_STK_CUBIC.h"
#include "Cubic.h"

EMFB_STK_API void * emfb_stk_cubic_new(char **exception_desc) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::Cubic());
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

EMFB_STK_API void emfb_stk_cubic_setA1(void *cubic, double a1) {
	static_cast<stk::Cubic *>(cubic)->setA1(a1);
}

EMFB_STK_API void emfb_stk_cubic_setA2(void *cubic, double a2) {
	static_cast<stk::Cubic *>(cubic)->setA2(a2);
}

EMFB_STK_API void emfb_stk_cubic_setA3(void *cubic, double a3) {
	static_cast<stk::Cubic *>(cubic)->setA3(a3);
}

EMFB_STK_API void emfb_stk_cubic_setGain(void *cubic, double gain) {
	static_cast<stk::Cubic *>(cubic)->setGain(gain);
}

EMFB_STK_API void emfb_stk_cubic_setThreshold(void *cubic, double threshold) {
	static_cast<stk::Cubic *>(cubic)->setThreshold(threshold);
}

EMFB_STK_API void emfb_stk_cubic_tickInplace(void *cubic, void *frames, unsigned int channel) {
	static_cast<stk::Cubic *>(cubic)->tick(*static_cast<stk::StkFrames *>(frames), channel);
}

EMFB_STK_API void emfb_stk_cubic_tick(void *cubic, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	static_cast<stk::Cubic *>(cubic)->tick(*static_cast<stk::StkFrames *>(iFrames), *static_cast<stk::StkFrames *>(oFrames), iChannel, oChannel);
}

EMFB_STK_API void emfb_stk_cubic_delete(void *cubic) {
	delete static_cast<stk::Cubic *>(cubic);
}