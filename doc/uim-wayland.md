# uim-wayland

`uim-wayland` is an input method for Wayland compositors that implement
`zwp_input_method_v1`: KWin (Plasma) and Weston. It makes uim usable in
applications that talk to the compositor with `text-input` instead of
loading a GTK or Qt input module, for example Chromium and Electron
applications running natively on Wayland.

With `zwp_input_method_v1` the compositor starts the input method
process itself and hands it a context whenever a text field gets
focus. Key events arrive through a keyboard grab, go through libuim,
and the results are sent back as preedit and committed text. Keys uim
doesn't consume are forwarded to the focused application.

GNOME's Mutter doesn't implement the input-method side of the
protocol, so `uim-wayland` can't be used there.

## Build

`uim-wayland` is built when `wayland-client`, `wayland-protocols`,
`wayland-scanner` and `xkbcommon` are found.
Pass `--without-wayland` to `configure` to disable it.

## KWin

Select "uim" as the virtual keyboard in System Settings, or set it in
`~/.config/kwinrc`:

```ini
[Wayland]
InputMethod=/usr/share/applications/uim-wayland.desktop
```

For testing inside another session, run a nested KWin and pass the
input method on the command line:

```sh
KWIN_DEVICE_SKIPS_INPUT_METHOD=none \
  dbus-run-session kwin_wayland --xwayland --socket=wayland-1 \
  --inputmethod /usr/bin/uim-wayland konsole
```

Applications started with `WAYLAND_DISPLAY=wayland-1` then use uim.

`KWIN_DEVICE_SKIPS_INPUT_METHOD` must be set to something (any value
that isn't a device name) in a nested session. KWin compares the
variable's comma-separated list with the name of the device a key
came from, and the devices of the nested Wayland backend have an
empty name, which matches the empty list. Without the variable KWin
still sends modifiers to the input method but never any key.

## Weston

Set the path in `weston.ini`:

```ini
[input-method]
path=/usr/bin/uim-wayland
```

Weston only implements `text-input-unstable-v1` on the application
side, so use `weston-editor` to test.

## Debugging

`UIM_WAYLAND_DEBUG=1` in the environment of `uim-wayland` logs
activation, the selected input method and every key with its uim
key code and whether uim consumed it. `WAYLAND_DEBUG=1` prints every
protocol message.

KWin passes its own environment to the input method, so set the
variables on `kwin_wayland` itself:

```sh
UIM_WAYLAND_DEBUG=1 dbus-run-session kwin_wayland ... --inputmethod /usr/bin/uim-wayland
```

`KWIN_IM_WAYLAND_DEBUG=1` makes KWin set `WAYLAND_DEBUG=1` for the
input method. Don't put variables in the `--inputmethod` value: KWin
splits it into a program and arguments, so `VAR=1 /usr/bin/uim-wayland`
tries to run a program called `VAR=1` and nothing starts. Weston's
`path=` does accept `VAR=1 /usr/bin/uim-wayland`.

The messages `uim-wayland` prints go to the stderr of the compositor.
If the input method doesn't start, look there first.

## Limitations

- A pending preedit is dropped when the text field loses focus.
  Applications differ in what they do with it (Chromium commits it),
  and `zwp_input_method_v1` gives no way to tell them anything after
  deactivation.
- Key repeat depends on the compositor. `uim-wayland` doesn't repeat
  keys itself, it only acts on the repeat events it receives while
  holding the keyboard grab. KWin sends them when the focused
  application relies on compositor-side repeat, and asks the input
  method to do the repeating otherwise, in which case keys don't
  repeat.
- No candidate window, so input methods that need one to pick between
  candidates can only be used for what they show in the preedit.
- Surrounding text from the application isn't passed to uim yet.
- uim-helper-server isn't used, so the toolbar and switching the input
  method from another process have no effect.
- `text-input-v3` has no preedit styling, so applications using it,
  Chromium among them, show the preedit without underlines. KWin does
  turn the highlighted segment into a selection range, so the segment
  being converted is still marked. Applications on `text-input-v1` or
  `text-input-v2`, which includes Qt applications under KWin, get the
  styling as sent.
