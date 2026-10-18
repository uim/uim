dict_gtk4_sources = [
  'canna-cclass.c',
  'cclass-dialog-gtk4.c',
  'dict.c',
  'main-gtk4.c',
  'util.c',
  'word-list-view-gtk4.c',
  'word-list-win-gtk4.c',
  'word-win-gtk4.c',
  'word.c',
]
dict_gtk4_c_args = ['-DUIM_DATADIR="@0@"'.format(prefix / pkgdatadir)]
if dict_anthy.found()
  dict_gtk4_sources += ['anthy.c']
  dict_gtk4_c_args += ['-DUSE_ANTHY']
endif
if dict_canna.found()
  dict_gtk4_sources += ['canna.c']
  dict_gtk4_c_args += ['-DUSE_CANNA']
endif
executable('uim-dict-gtk4',
           dict_gtk4_sources,
           c_args: dict_gtk4_c_args,
           dependencies: [gtk4, dict_anthy, dict_canna],
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim])
