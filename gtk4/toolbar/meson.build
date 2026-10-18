executable('uim-toolbar-gtk4',
           'standalone-gtk4.c',
           'uim-toolbar-widget.c',
           dependencies: [gtk4, gtk4_layer_shell, x11],
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim, libuim_custom])
