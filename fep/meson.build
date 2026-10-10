# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
fep_c_args = ['-DHAVE_CONFIG_H']
# They use libreplace, which Autotools links through libuim.
executable('uim-fep',
           'callbacks.c',
           'draw.c',
           'escseq.c',
           'helper.c',
           'key.c',
           'read.c',
           'str.c',
           'udsock.c',
           'uim-fep.c',
           c_args: fep_c_args,
           dependencies: [curses] + forkpty_dependencies,
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim, libreplace])
executable('uim-fep-tick',
           'udsock.c',
           'uim-fep-tick.c',
           c_args: fep_c_args,
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim, libreplace])
