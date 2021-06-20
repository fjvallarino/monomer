#if !defined(_WIN32)

void initializeDpiAwareness() {
}

#else

#include "windows.h"
#include "sdl.h"

typedef enum PROCESS_DPI_AWARENESS {
    PROCESS_DPI_UNAWARE = 0,
    PROCESS_SYSTEM_DPI_AWARE = 1,
    PROCESS_PER_MONITOR_DPI_AWARE = 2
} PROCESS_DPI_AWARENESS;

BOOL(WINAPI *SetProcessDPIAwareFn)(void); // Vista and later
HRESULT(WINAPI *SetProcessDpiAwarenessFn)(PROCESS_DPI_AWARENESS dpiAwareness); // Windows 8.1 and later

void initializeDpiAwareness() {
    void* userDLL = SDL_LoadObject("USER32.DLL");
    void* shcoreDLL = SDL_LoadObject("SHCORE.DLL");

    if (userDLL) {
        SetProcessDPIAwareFn = (BOOL(WINAPI *)(void)) SDL_LoadFunction(userDLL, "SetProcessDPIAware");
    }

    if (shcoreDLL) {
        SetProcessDpiAwarenessFn = (HRESULT(WINAPI *)(PROCESS_DPI_AWARENESS)) SDL_LoadFunction(shcoreDLL, "SetProcessDpiAwareness");
    }

    if (SetProcessDpiAwarenessFn) {
        // Try Windows 8.1+ version
        //HRESULT result = SetProcessDpiAwarenessFn(PROCESS_PER_MONITOR_DPI_AWARE);
        HRESULT result = SetProcessDpiAwarenessFn(PROCESS_SYSTEM_DPI_AWARE);
        SDL_Log("called SetProcessDpiAwareness: %d", (result == S_OK) ? 1 : 0);
    }
    else if (SetProcessDPIAwareFn) {
        // Try Vista - Windows 8 version.
        // This has a constant scale factor for all monitors.
        BOOL success = SetProcessDPIAwareFn();
        SDL_Log("Called SetProcessDPIAware: %d", (int) success);
    }
}

#endif
