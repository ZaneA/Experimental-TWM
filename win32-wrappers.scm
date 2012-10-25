;;;
;;; Win32 API wrappers
;;;

;; Headers / Defines

(foreign-declare "
#define WIN32_LEAN_AND_MEAN
#define WINVER        0x0501
#define _WIN32_WINNT  0x0501
#include <windows.h>
")


;;
;; Utility
;;

(define* win32/message-box
  "Show a message box."
  (foreign-lambda* void ((c-string message) (c-string title))
   "MessageBox(NULL, message, title, MB_OK);"))


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
  (win32-open-console)
  (proc)
  (win32-close-console))

; Macro as syntactic sugar
(define-syntax win32/with-console
  (syntax-rules ()
    [_ body ...] (win32/with-console* (lambda () body ...))))

(define* win32/print-to-console
  "Print to an open console."
  (foreign-lambda* void ((c-string line))
   "HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (out) {
      SetConsoleTitle(\"HashTWM3 REPL\");
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

(define* win32/get-window-title
  "Get the window title from the provided HWND."
  (foreign-lambda* c-string ((c-int hwnd))
    "static char title[256];
     GetWindowText(hwnd, title, sizeof(title));
     C_return(&title);"))

(define* win32/get-window-class
  "Get the window class from the provided HWND."
  (foreign-lambda* c-string ((c-int hwnd))
    "static char class;
     GetClassName(hwnd, class sizeof(class));
     C_return(&class);"))
