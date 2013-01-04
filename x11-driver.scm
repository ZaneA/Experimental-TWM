;;;
;;; X11 Driver
;;;

;; Headers / Defines

(foreign-declare "#include <xcb/xcb.h>")


;;
;; API
;;

(define* driver/get-window-title
  "Get the window title from the provided handle."
  (foreign-lambda* c-string ((c-pointer hwnd))
    "static char title[256];
     C_return(&title);"))

(define* driver/get-window-class
  "Get the window class from the provided handle."
  (foreign-lambda* c-string ((c-pointer hwnd))
    "static char class[256];
     C_return(&class);"))

(define* driver/main-loop
  "Driver main loop."
  (foreign-safe-lambda* void ()
    "xcb_connection_t *dpy;
     xcb_screen_t *screen;
     xcb_drawable_t win;
     xcb_drawable_t root;
     
     xcb_generic_event_t *ev;
     xcb_get_geometry_reply_t *geom;
     
     dpy = xcb_connect(NULL, NULL);
     
     screen = xcb_setup_roots_iterator(xcb_get_setup(dpy)).data;
     root = screen->root;
     
     for (;;) {
       ev = xcb_wait_for_event(dpy);
       switch (ev->response_type & ~0x80) {
         case XCB_MOTION_NOTIFY:
         {
           xcb_query_pointer_reply_t *pointer;
           pointer = xcb_query_pointer_reply(dpy, xcb_query_pointer(dpy, root), 0);
           geom = xcb_get_geometry_reply(dpy, xcb_get_geometry(dpy, win), NULL);
           xcb_configure_window(dpy, win, XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y | XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT, NULL);
           xcb_flush(dpy);
         }
         break;
       }
     }"))
