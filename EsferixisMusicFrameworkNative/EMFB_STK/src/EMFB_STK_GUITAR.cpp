#include "EMFB_STK_GUITAR.h"

#include <Stk.h>
#include <Guitar.h>

void * emfb_stk_guitar_new(char **exception_desc, unsigned int nStrings, char *bodyfile) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::Guitar(nStrings, std::string(bodyfile)));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_guitar_delete(char **exception_desc, void *guitar) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	delete static_cast<stk::Guitar *>(guitar);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_clear(char **exception_desc, void *guitar) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->clear();
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_setLoopGain(char **exception_desc, void *guitar, double gain, int string) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->setLoopGain(gain, string);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_setPluckPosition(char **exception_desc, void *guitar, double position, int string) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->setPluckPosition(position, string);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_setFrequency(char **exception_desc, void *guitar, double frequency, unsigned int string) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->setFrequency(frequency, string);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_noteOn(char **exception_desc, void *guitar, double frequency, double amplitude, unsigned int string) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->noteOn(frequency, amplitude, string);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_noteOff(char **exception_desc, void *guitar, double amplitude, unsigned int string) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->noteOff(amplitude, string);
	EMFB_STK_CATCHEXCEPT_END
}

void emfb_stk_guitar_tick(char **exception_desc, void *guitar, void *iframes, void *oframes, unsigned int iChannel, unsigned int oChannel) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Guitar *>(guitar)->tick( *static_cast<stk::StkFrames *>(iframes), *static_cast<stk::StkFrames *>(oframes), iChannel, oChannel);
	EMFB_STK_CATCHEXCEPT_END
}