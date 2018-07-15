#include "EMFB_STK_FILEWVOUT.h"

#include <Stk.h>
#include <FileWvOut.h>
#include <string>

void * emfb_stk_filewvout_create() {
	return static_cast<void *>(new stk::FileWvOut());
}

void emfb_stk_filewvout_delete(void *fileWvOut) {
	delete static_cast<stk::FileWvOut *>(fileWvOut);
}

void emfb_stk_filewvout_openFile(void *fileWvOut, char **exception_desc, char *fileName, unsigned int nChannels, unsigned long type, unsigned long format) {
	*exception_desc = NULL;

	try {
		static_cast<stk::FileWvOut *>(fileWvOut)->openFile(std::string(fileName), nChannels, type, format);
	}
	catch (stk::StkError e) {
		*exception_desc = emfb_stk_cppStrToCStr(e.getMessage());
	}
	
}

void emfb_stk_filewvout_tick(void *fileWvOut, void *frames) {
	static_cast<stk::FileWvOut *>(fileWvOut)->tick( *( (static_cast<stk::StkFrames *>(frames) ) ) );
}