# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
executable('uim-xim',
           'canddisp.cpp',
           'compose.cpp',
           'connection.cpp',
           'convdisp.cpp',
           'helper.cpp',
           'locale.cpp',
           'main.cpp',
           'util.cpp',
           'ximic.cpp',
           'ximim.cpp',
           'ximpacket.cpp',
           'ximserver.cpp',
           'ximtrans.cpp',
           cpp_args: ['-DHAVE_CONFIG_H'],
           dependencies: [x11, xext, xft],
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim, libuim_x_util])
install_man('uim-xim.1')
