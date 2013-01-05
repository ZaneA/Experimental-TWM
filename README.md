
    HashTWM3 ->
    Rising from the dead.

This is another experimental TWM that will have most of the "business logic" written in Chicken Scheme, while having the low-level manipulation performed by platform drivers written in C.

Running
---

There is no Makefile yet, but if you have [chicken](http://www.call-cc.org) installed you should be able to compile for either platform with some minimal modifications to the `compile.sh` script.

Hacking
---

`hashtwm3.scm` is the main entry point. It loads up the driver API and launches the driver main-loop.

`drivers/api.scm` defines the API that will eventually be used by the "business logic". There is lots still to be added here.

`drivers/win32/driver.c` is the Win32 driver. It contains a traditional event loop with a message-only window. Right now no events are handled and the `WndProc` is simply deferring to `DefWindowProc`.

`drivers/x11/driver.c` is the X11 driver. It is built on top of the XCB API and listens for `SubstructureNotify` events on the root window.

`docstrings.scm` is a small macro for defining procedures/variables with "docstrings". The `(help)` procedure currently doesn't work here due to a change in Chicken since an older version where the code was originally written.

License
---

GPLv3.
