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

Clicking an icon switches to the next one of what it shows, such as
the next input mode, and scrolling on it switches to the next or the
previous one. Right-clicking it shows the menu of them, such as the
input methods and the input modes, and choosing one switches to it.

The scroll direction depends on the panel: scrolling down switches to
the next one in swaybar and Waybar, and scrolling up does in KDE
Plasma.

Turn on "Use icon for dark background"
(`toolbar-icon-for-dark-background?`) for a panel with a dark
background, such as Waybar by default. The change applies when
uim-pref saves it, or with `uim-custom-cli`:

```console
$ uim-custom-cli --save 'toolbar-icon-for-dark-background?' '#t'
```

Only one `uim-sni` runs in a session. It owns `org.uim.Sni` on the
session bus, and another `uim-sni` exits.

## Limitations

- swaybar doesn't show the menus: it doesn't support
  `com.canonical.dbusmenu`. Click or scroll on an icon to switch
  there.
- The menus have no icons.
- The menus don't have the commands of the other toolbars, such as
  starting the input method switcher and the preferences.
