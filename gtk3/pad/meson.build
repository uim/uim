executable('uim-input-pad-ja-gtk3',
           'ja.c',
           # ja.c includes config.h only when HAVE_CONFIG_H is defined as
           # Autotools does.
           c_args: ['-DHAVE_CONFIG_H'],
           dependencies: gtk3,
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim])
