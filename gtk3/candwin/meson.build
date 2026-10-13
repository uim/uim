# [suffix, source, extra c_args]
foreach definition : [
  ['', 'uim-cand-win-vertical-gtk.c', []],
  ['-horizontal',
   'uim-cand-win-horizontal-gtk.c',
   ['-DUIM_CANDWIN_STYLE_HORIZONTAL']],
  ['-tbl', 'uim-cand-win-tbl-gtk.c', ['-DUIM_CANDWIN_STYLE_TABLE']],
]
  executable('uim-candwin' + definition[0] + '-gtk3',
             'gtk.c',
             '../immodule/caret-state-indicator.c',
             '../immodule/uim-cand-win-gtk.c',
             '../immodule/' + definition[1],
             # gtk.c includes config.h only when HAVE_CONFIG_H is
             # defined as Autotools does.
             c_args: ['-DHAVE_CONFIG_H'] + definition[2],
             dependencies: gtk3,
             include_directories: uim_include_directories,
             install: true,
             install_dir: libexecdir,
             link_with: [libuim_scm, libuim])
endforeach
