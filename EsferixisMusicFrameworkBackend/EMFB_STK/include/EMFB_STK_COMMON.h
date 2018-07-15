#pragma once

#ifdef _WIN32
	#if EMFB_STK_EXPORTS
		#define EMFB_STK_API extern "C" __declspec(dllexport)
	#else
		#define EMFB_STK_API extern "C" __declspec(dllimport)
	#endif
#else
	#define EMFB_STK extern "C"
#endif