executable('uim-pref-gtk3',
           'gtk-custom-widgets.c',
           'gtk.c',
           '../immodule/key-util-gtk.c',
           dependencies: gtk3,
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_custom, libuim_scm, libuim, libuim_x_util])
