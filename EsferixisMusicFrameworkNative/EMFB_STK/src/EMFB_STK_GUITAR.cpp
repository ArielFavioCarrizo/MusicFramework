#include "EMFB_STK_GUITAR.h"

#include <Stk.h>
#include <Guitar.h>

void * emfb_stk_guitar_new(char **exception_desc, unsigned int nStrings, char *bodyfile) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::Guitar(nStrings, std::string(bodyfile)));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_guitar_delete(void *guitar) {
	delete static_cast<stk::Guitar *>(guitar);
}

void emfb_stk_guitar_clear(void *guitar) {
	static_cast<stk::Guitar *>(guitar)->clear();
}

void emfb_stk_guitar_setLoopGain(void *guitar, double gain, int string) {
	static_cast<stk::Guitar *>(guitar)->setLoopGain(gain, string);
}

void emfb_stk_guitar_setPluckPosition(void *guitar, double position, int string) {
	static_cast<stk::Guitar *>(guitar)->setPluckPosition(position, string);
}

void emfb_stk_guitar_setFrequency(void *guitar, double frequency, unsigned int string) {
	static_cast<stk::Guitar *>(guitar)->setFrequency(frequency, string);
}

void emfb_stk_guitar_noteOn(void *guitar, double frequency, double amplitude, unsigned int string) {
	static_cast<stk::Guitar *>(guitar)->noteOn(frequency, amplitude, string);
}

void emfb_stk_guitar_noteOff(void *guitar, double amplitude, unsigned int string) {
	static_cast<stk::Guitar *>(guitar)->noteOff(amplitude, string);
}

void emfb_stk_guitar_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::Guitar>(self, iFrames, oFrames, iChannel, oChannel);
}