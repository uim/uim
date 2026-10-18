gtk4_pref_link_with = [libuim_custom, libuim_scm, libuim]
if x11.found()
  gtk4_pref_link_with += [libuim_x_util]
endif
executable('uim-pref-gtk4',
           'gtk-custom-widgets-gtk4.c',
           'gtk-gtk4.c',
           'key-util-gtk4.c',
           c_args: ['-DUIM_DATADIR="@0@"'.format(prefix / pkgdatadir)],
           dependencies: [gtk4, x11],
           include_directories: uim_include_directories,
           install: true,
           link_with: gtk4_pref_link_with)
