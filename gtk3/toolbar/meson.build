# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
gtk3_toolbar_c_args = ['-DHAVE_CONFIG_H']
gtk3_toolbar_link_with = [libuim_scm, libuim, libuim_custom]
executable('uim-toolbar-gtk3',
           'common-gtk.c',
           'standalone-gtk.c',
           c_args: gtk3_toolbar_c_args,
           dependencies: gtk3,
           include_directories: uim_include_directories,
           install: true,
           link_with: gtk3_toolbar_link_with)
executable('uim-toolbar-gtk3-systray',
           'common-gtk.c',
           'eggtrayicon.c',
           'systray-gtk.c',
           c_args: gtk3_toolbar_c_args,
           dependencies: [gtk3, x11],
           include_directories: uim_include_directories,
           install: true,
           link_with: gtk3_toolbar_link_with)
