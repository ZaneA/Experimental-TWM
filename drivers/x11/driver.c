//
// X11 Driver
//

#include <xcb/xcb.h>
#include <xcb/xproto.h>
#include <stdio.h>

// driver/display-info
void driver_display_info(char *message)
{
   printf("info: %s\n", message);
}

// driver/display-error
void driver_display_error(char *message)
{
   printf("error: %s\n", message);
}

// driver/get-last-error
char* driver_get_last_error()
{
  return "UNIMPLEMENTED";
}

// driver/get-window-title
char* driver_get_window_title(void* handle)
{
  static char title[256];

  return "UNIMPLEMENTED";
}

// driver/get-window-class
char* driver_get_window_class(void* handle)
{
  static char class[256];

  return "UNIMPLEMENTED";
}

// driver/main-loop
void driver_main_loop()
{
  xcb_connection_t *dpy;
  xcb_screen_t *screen;
  xcb_drawable_t root;

  xcb_generic_event_t *ev;

  dpy = xcb_connect(NULL, NULL);

  screen = xcb_setup_roots_iterator(xcb_get_setup(dpy)).data;
  root = screen->root;

  // Ask to be notified of changes to root window substructures.
  uint32_t values[2];
  values[0] = XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY;
  xcb_change_window_attributes_checked(dpy, root, XCB_CW_EVENT_MASK, values);
  xcb_flush(dpy);

  for (;;) {
    ev = xcb_wait_for_event(dpy);

    switch (ev->response_type & ~0x80) {
      // Window has been mapped.
      case XCB_MAP_NOTIFY:
        {
          // TODO This should only fire for window frame, not child as well.
          if (((xcb_map_notify_event_t*)ev)->event == root) {
            printf("Window was created %d\n", ((xcb_map_notify_event_t*)ev)->window);
          }
        }
        break;

      // Window has been unmapped.
      case XCB_UNMAP_NOTIFY:
        {
          if (((xcb_unmap_notify_event_t*)ev)->event == root) {
            printf("Window was destroyed %d\n", ((xcb_unmap_notify_event_t*)ev)->window);
          }
        }
        break;
    }
  }
}
