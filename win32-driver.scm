;;;
;;; Win32 API wrappers
;;;

;; Headers / Defines

(foreign-declare "
#define WIN32_LEAN_AND_MEAN
#define WINVER 0x0501
#define _WIN32_WINNT 0x0501
#include <windows.h>
#define APP_NAME \"HashTWM3\"
")

;(define-foreign-type lpstr "LPSTR")


;;
;; Utility
;;

; Wrapped to allow keyword arguments.
(define* win32/message-box
  "Show a message box."
  (let ((%message-box (foreign-lambda* void ((c-string message) (c-string title))
                      "MessageBox(NULL, message, title, MB_OK);")))
    (lambda (message #!key (title "Message"))
      (%message-box message title))))


;;
;; Console
;;

(define* win32/open-console
  "Allocate a console and display it."
  (foreign-lambda void "AllocConsole")) 

(define* win32/close-console
  "Close and free a console."
  (foreign-lambda void "FreeConsole"))

; Helper for doing something with an open console
(define* (win32/with-console* proc)
  "Helper for doing something with an open console."
  (win32/open-console)
  (proc)
  (win32-close-console))

; Macro as syntactic sugar
(define-syntax win32/with-console
  (syntax-rules ()
    [(_ body ...) (win32/with-console* (lambda () body ...))]))

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

; Override Chicken Console
;(current-output-port
;  (make-output-port
;    win32/print-to-console
;    (lambda () #f)))

;;
;; Windows
;;

(define* win32/get-last-error
  "Retrieve the calling thread's last-error as a string"
  (foreign-lambda int "GetLastError"))

(define* driver/get-window-title
  "Get the window title from the provided HWND."
  (foreign-lambda* c-string ((c-pointer hwnd))
    "static char title[256];
     GetWindowText(hwnd, title, sizeof(title));
     C_return(&title);"))

(define* driver/get-window-class
  "Get the window class from the provided HWND."
  (foreign-lambda* c-string ((c-pointer hwnd))
    "static char class[256];
     GetClassName(hwnd, class, sizeof(class));
     C_return(&class);"))


;;
;; Low level Windows setup and event loop
;;

; Internally used to forward messages to the user modifiable proc
(define-external (FailureCB (c-string err)) void
  (failure-callback err))

(define* win32/def-wnd-proc
  "Return default window proc."
  (foreign-lambda long "DefWindowProc" c-pointer unsigned-int unsigned-int long))

(define* (win32/process-wnd-proc hwnd msg w-param l-param)
  "WndProc implementation."
  (printf "HWND: ~a, MSG: ~a, WPARAM: ~a, LPARAM: ~a~n" hwnd msg w-param l-param)
  (win32/def-wnd-proc hwnd msg w-param l-param))

; Internally used to forward messages to the user modifiable proc
(define-external (WndProc (c-pointer hwnd) (unsigned-int msg) (unsigned-int w-param) (long l-param)) long
  (win32/process-wnd-proc hwnd msg w-param l-param))

(define* driver/main-loop
  "Win32 API main loop."
  (foreign-safe-lambda* void ()
    "HWND hwnd;
     WNDCLASSEX winClass = { 0 };
     winClass.cbSize = sizeof(WNDCLASSEX);
     winClass.lpfnWndProc = WndProc;
     winClass.hInstance = GetModuleHandle(NULL);
     winClass.lpszClassName = APP_NAME;
     
     if (!RegisterClassEx(&winClass)) {
       FailureCB(\"Error Registering Window Class\");
       C_return(0);
     }
     
     hwnd = CreateWindowEx(0, APP_NAME, APP_NAME, 0, 0, 0, 0, 0, HWND_MESSAGE, NULL, GetModuleHandle(NULL), NULL);
     
     if (!hwnd) {
       FailureCB(\"Error Creating Window\");
       C_return(0);
     }

     MSG msg;
     while (GetMessage(&msg, NULL, 0, 0) > 0) {
       TranslateMessage(&msg);
       DispatchMessage(&msg);
     }"))