//
// Win32 Driver
//

#define WIN32_LEAN_AND_MEAN
#define WINVER 0x0501
#define _WIN32_WINNT 0x0501
#include <windows.h>
#include <stdio.h>
#define APP_NAME "HashTWM3"

// driver/display-info
void driver_display_info(char *message)
{
  printf("info: %s\n", message);
}

// driver/display-error
void driver_display_error(char *message)
{
  printf("error: %s\n", message);
  MessageBox(NULL, message, "Error", MB_ICONERROR | MB_OK);
}

/*
(define* win32/open-console
  "Allocate a console and display it."
  (foreign-lambda void "AllocConsole")) 

(define* win32/close-console
  "Close and free a console."
  (foreign-lambda void "FreeConsole"))

(define* win32/print-to-console
  "Print to an open console."
  (foreign-lambda* void ((c-string line))
   "HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (out) {
      SetConsoleTitle(APP_NAME\" REPL\");
      DWORD written;
      WriteConsole(out, line, strlen(line), &written, NULL);
    }"))

(define* win32/read-from-console
  "Read from an open console."
  (foreign-lambda* c-string ()
   "static char line[256];
    DWORD charsRead = 0;
    
    memset(line, 0, sizeof(line));
    
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    if (in) {
      ReadConsole(in, &line, sizeof(line), &charsRead, NULL);
    }
    
    C_return(&line);"))
    */

// driver/get-last-error
char* driver_get_last_error()
{
  static char error[256];

  // TODO Get the actual message as a string.
  sprintf(error, "GetLastError: %i", GetLastError());

  return error;
}

// driver/get-window-title
char* driver_get_window_title(void* handle)
{
  static char title[256];

  GetWindowText((HWND)handle, title, sizeof(title));

  return title;
}

// driver/get-window-class
char* driver_get_window_class(void* handle)
{
  static char class[256];

  GetClassName((HWND)handle, class, sizeof(class));

  return class;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  // Not handling anything for now, so return default proc.
  return DefWindowProc(hwnd, msg, wParam, lParam);
}

// driver/main-loop
void driver_main_loop()
{
  HWND hwnd;
  WNDCLASSEX winClass = { 0 };
  winClass.cbSize = sizeof(WNDCLASSEX);
  winClass.lpfnWndProc = WndProc;
  winClass.hInstance = GetModuleHandle(NULL);
  winClass.lpszClassName = APP_NAME;

  // Register window class.
  if (!RegisterClassEx(&winClass)) {
    driver_display_error("Error Registering Window Class");
    return;
  }

  // Create message only window.
  hwnd = CreateWindowEx(0, APP_NAME, APP_NAME, 0, 0, 0, 0, 0, HWND_MESSAGE, NULL, GetModuleHandle(NULL), NULL);

  if (!hwnd) {
    driver_display_error("Error Creating Window");
    return;
  }

  MSG msg;
  // Run Win32 event loop.
  while (GetMessage(&msg, NULL, 0, 0) > 0) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}
