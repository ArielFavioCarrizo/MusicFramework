#include "EMFB_STK_DELAYA.h"

#include <Stk.h>
#include <DelayA.h>

void * emfb_stk_delaya_new(char **exception_desc, unsigned long delay, unsigned long maxDelay) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::DelayA(delay, maxDelay));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void emfb_stk_delaya_delete(void *self) {
	delete static_cast<stk::DelayA *>(self);
}

void emfb_stk_delaya_clear(void *self) {
	static_cast<stk::DelayA *>(self)->clear();
}

unsigned long emfb_stk_delaya_getMaximumDelay(void *self) {
	return static_cast<stk::DelayA *>(self)->getMaximumDelay();
}

void emfb_stk_delaya_setMaximumDelay(void *self, unsigned long delay) {
	static_cast<stk::DelayA *>(self)->setMaximumDelay(delay);
}

void emfb_stk_delaya_setDelay(char **exception_desc, void *self, unsigned long delay) {
	static_cast<stk::DelayA *>(self)->setDelay(delay);
}

unsigned long emfb_stk_delaya_getDelay(void *self) {
	return static_cast<stk::DelayA *>(self)->getDelay();
}

void emfb_stk_delaya_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::DelayA>(self, frames, channel);
}

void emfb_stk_delaya_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::DelayA>(self, iFrames, oFrames, iChannel, oChannel);
}