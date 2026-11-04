# [suffix, source, extra c_args]
foreach definition : [
  ['', 'uim-cand-win-vertical-gtk4.c', []],
  ['-horizontal',
   'uim-cand-win-horizontal-gtk4.c',
   ['-DUIM_CANDWIN_STYLE_HORIZONTAL']],
  ['-tbl', 'uim-cand-win-tbl-gtk4.c', ['-DUIM_CANDWIN_STYLE_TABLE']],
]
  executable('uim-candwin' + definition[0] + '-gtk4',
             'cand-win-x11-util.c',
             'caret-state-indicator-gtk4.c',
             'gtk-gtk4.c',
             'uim-cand-win-gtk4.c',
             definition[1],
             c_args: definition[2],
             dependencies: [gtk4, x11],
             include_directories: uim_include_directories,
             install: true,
             install_dir: libexecdir,
             link_with: [libuim_scm, libuim])
endforeach
