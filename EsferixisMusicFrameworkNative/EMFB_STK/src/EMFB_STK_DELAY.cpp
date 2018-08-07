#include "EMFB_STK_DELAY.h"

#include <Stk.h>
#include <Delay.h>

void * emfb_stk_delay_new(char **exception_desc, unsigned long delay, unsigned long maxDelay) {
	return static_cast<void *>(new stk::Delay(delay, maxDelay));
}

void emfb_stk_delay_delete(void *self) {
	delete static_cast<stk::Delay *>(self);
}

unsigned long emfb_stk_delay_getMaximumDelay(void *self) {
	return static_cast<stk::Delay *>(self)->getMaximumDelay();
}

void emfb_stk_delay_setMaximumDelay(void *self, unsigned long delay) {
	static_cast<stk::Delay *>(self)->setMaximumDelay(delay);
}

void emfb_stk_delay_setDelay(char **exception_desc, void *self, unsigned long delay) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	static_cast<stk::Delay *>(self)->setDelay(delay);
	EMFB_STK_CATCHEXCEPT_END
}

unsigned long emfb_stk_delay_getDelay(void *self) {
	return static_cast<stk::Delay *>(self)->getDelay();
}

void emfb_stk_delay_tickInplace(void *self, void *frames, unsigned int channel) {
	emfb_stk_tickInplace<stk::Delay>(self, frames, channel);
}

void emfb_stk_delay_tick(void *self, void *iFrames, void *oFrames, unsigned int iChannel, unsigned int oChannel) {
	emfb_stk_tick<stk::Delay>(self, iFrames, oFrames, iChannel, oChannel);
}