shared_module('im-uim',
              'gtk-im-uim.c',
              'uim-candidates-view.c',
              'uim-im-context.c',
              dependencies: [gtk4, cc.find_library('m', required: false)],
              include_directories: uim_include_directories,
              install: true,
              install_dir: (libdir / 'gtk-4.0' /
                            gtk4.get_variable(pkgconfig: 'gtk_binary_version') /
                            'immodules'),
              link_with: [libuim_scm, libuim, libuim_counted_init])
