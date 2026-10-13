gtk3_immodule_dir = (libdir / 'gtk-3.0' /
                     gtk3.get_variable(pkgconfig: 'gtk_binary_version') /
                     'immodules')
shared_module('im-uim',
              'caret-state-indicator.c',
              'compose.c',
              'gtk-im-uim.c',
              'key-util-gtk.c',
              'text-util.c',
              'uim-cand-win-gtk.c',
              'uim-cand-win-horizontal-gtk.c',
              'uim-cand-win-tbl-gtk.c',
              'uim-cand-win-vertical-gtk.c',
              dependencies: [gtk3, x11, cc.find_library('m', required: false)],
              include_directories: uim_include_directories,
              install: true,
              install_dir: gtk3_immodule_dir,
              link_with: [libuim_scm,
                          libuim,
                          libuim_counted_init,
                          libuim_x_util],
              name_prefix: '')

# GTK finds immodules in its libdir without GTK_PATH.
gtk3_path = prefix / libdir / 'gtk-3.0'
if prefix / libdir == gtk3.get_variable(pkgconfig: 'libdir')
  gtk3_path = ''
endif
meson.add_install_script('update-immodules-cache.sh',
                         'gtk-query-immodules-3.0',
                         gtk3_path)
