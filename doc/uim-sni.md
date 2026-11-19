# uim-sni

`uim-sni` shows the uim status in the trays of panels that support
StatusNotifierItem, such as Waybar, swaybar and KDE Plasma. It is the
toolbar for these panels: each item of the toolbar, the input method,
the input mode and so on, is shown as an icon in the tray as
`uim-toolbar-gtk3-systray` shows each one as a button in an XEmbed
tray. Trays of the panels for Wayland compositors don't support XEmbed.

`uim-sni` talks to the panel over the D-Bus session bus and to the
input methods through uim-helper-server, so it needs neither GTK nor
Qt.

## Build

`uim-sni` is built when `gio-2.0` is found. Pass `-Dsni=disabled` to
`meson setup` to disable it. Autotools doesn't build it.

## Usage

Start it in the session, for instance in the Sway configuration:

```
exec uim-sni
```

The icons are added when the panel's tray is available and are added
again when the panel restarts.

Only one `uim-sni` runs in a session. It owns `org.uim.Sni` on the
session bus, and another `uim-sni` exits.

## Limitations

- Clicking an icon does nothing yet. Use `uim-im-switcher-cli` or
  another toolbar to switch the input method or the input mode.
- The icons for a dark background
  (`toolbar-icon-for-dark-background?`) aren't used yet.
