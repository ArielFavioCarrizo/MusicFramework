#include "EMFB_STK_FRAMES.h"
#include "Stk.h"

void checkSameDimensions(stk::StkFrames& frames1, stk::StkFrames& frames2) {
	if ( frames1.channels() != frames2.channels() ) {
		throw std::runtime_error("Channels mismatch");
	}

	if (frames1.frames() != frames2.frames()) {
		throw std::runtime_error("Frames mismatch");
	}
}

void * emfb_stk_stkframes_new_zero(char **exception_desc, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

void * emfb_stk_stkframes_new_valued(char **exception_desc, double value, unsigned int nFrames, unsigned int nChannels) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(value, nFrames, nChannels));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

unsigned int emfb_stk_stkframes_channels(void *frames) {
	return static_cast<stk::StkFrames *>(frames)->channels();
}

unsigned int emfb_stk_stkframes_nFrames(void *frames) {
	return static_cast<stk::StkFrames *>(frames)->frames();
}

void * emfb_stk_stkframes_clone(char **exception_desc, void *frames) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	return static_cast<void *>(new stk::StkFrames(*static_cast<stk::StkFrames *>(frames)));
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

EMFB_STK_API void * emfb_stk_stkframes_add(char **exception_desc, void *frames1, void *frames2) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	stk::StkFrames& frames1_ref = *static_cast<stk::StkFrames *>(frames1);
	stk::StkFrames& frames2_ref = *static_cast<stk::StkFrames *>(frames2);
	checkSameDimensions(frames1_ref, frames2_ref);

	stk::StkFrames *resultFrames = new stk::StkFrames(frames1_ref.frames(), frames1_ref.channels());

	stk::StkFloat *frames1Data = &frames1_ref[0];
	stk::StkFloat *frames2Data = &frames2_ref[0];
	stk::StkFloat *dstData = &( (*resultFrames)[0] );

	for (int i = 0; i < frames1_ref.size(); i++) {
		dstData[i] = frames1Data[i] + frames2Data[i];
	}

	return resultFrames;
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

EMFB_STK_API void * emfb_stk_stkframes_mulHomologs(char **exception_desc, void *frames1, void *frames2) {
	EMFB_STK_CATCHEXCEPT_BEGIN
    stk::StkFrames& frames1_ref = *static_cast<stk::StkFrames *>(frames1);
	stk::StkFrames& frames2_ref = *static_cast<stk::StkFrames *>(frames2);
	checkSameDimensions(frames1_ref, frames2_ref);

	stk::StkFrames *resultFrames = new stk::StkFrames(frames1_ref.frames(), frames1_ref.channels());

	stk::StkFloat *frames1Data = &frames1_ref[0];
	stk::StkFloat *frames2Data = &frames2_ref[0];
	stk::StkFloat *dstData = &((*resultFrames)[0]);

	for (int i = 0; i < frames1_ref.size(); i++) {
		dstData[i] = frames1Data[i] * frames2Data[i];
	}

	return resultFrames;
	EMFB_STK_CATCHEXCEPT_END
    return nullptr;
}

void emfb_stk_stkframes_addInplace(void *selfFrames, void *otherFrames) {
	stk::StkFrames& selfFrames_ref = *static_cast<stk::StkFrames *>(selfFrames);
	stk::StkFrames& otherFrames_ref = *static_cast<stk::StkFrames *>(otherFrames);

	selfFrames_ref += otherFrames_ref;
}

void emfb_stk_stkframes_mulHomologsInplace(void *selfFrames, void *otherFrames) {
	stk::StkFrames& selfFrames_ref = *static_cast<stk::StkFrames *>(selfFrames);
	stk::StkFrames& otherFrames_ref = *static_cast<stk::StkFrames *>(otherFrames);

	selfFrames_ref *= otherFrames_ref;
}

EMFB_STK_API void * emfb_stk_stkframes_scale(char **exception_desc, void *frames, double scalar) {
	EMFB_STK_CATCHEXCEPT_BEGIN
	stk::StkFrames& frames_ref = *static_cast<stk::StkFrames *>(frames);

	stk::StkFrames *resultFrames = new stk::StkFrames(frames_ref.frames(), frames_ref.channels());

	stk::StkFloat *selfFramesData = &frames_ref[0];
	stk::StkFloat *dstData = &((*resultFrames)[0]);

	for (int i = 0; i < frames_ref.size(); i++) {
		dstData[i] = selfFramesData[i] * scalar;
	}

	return resultFrames;
	EMFB_STK_CATCHEXCEPT_END
	return nullptr;
}

EMFB_STK_API void emfb_stk_stkframes_scaleInplace(void *frames, double scalar) {
	stk::StkFrames& frames_ref = *static_cast<stk::StkFrames *>(frames);

	stk::StkFloat *selfFramesData = &frames_ref[0];

	for (int i = 0; i < frames_ref.size(); i++) {
		selfFramesData[i] *= scalar;
	}
}

void emfb_stk_stkframes_delete(void *frames) {
	delete static_cast<stk::StkFrames *>(frames);
}