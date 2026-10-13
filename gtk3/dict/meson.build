dict_sources = [
  'cclass-dialog.c',
  'canna-cclass.c',
  'dict.c',
  'main-gtk.c',
  'util.c',
  'word-list-view-gtk.c',
  'word-list-win-gtk.c',
  'word-win-gtk.c',
  'word.c',
]
dict_c_args = ['-DUIM_DATADIR="@0@"'.format(prefix / pkgdatadir)]
if dict_anthy.found()
  dict_sources += ['anthy.c']
  dict_c_args += ['-DUSE_ANTHY']
endif
if dict_canna.found()
  dict_sources += ['canna.c']
  dict_c_args += ['-DUSE_CANNA']
endif
executable('uim-dict-gtk3',
           dict_sources,
           c_args: dict_c_args,
           dependencies: [gtk3, dict_anthy, dict_canna],
           include_directories: uim_include_directories,
           install: true,
           link_with: [libuim_scm, libuim])
# The menu items of the unavailable dictionaries are commented out.
configure_file(input: 'uim-dict-ui.xml.in',
               output: 'uim-dict-ui.xml',
               configuration: {
                 'UI_XML_ANTHY_START': dict_anthy.found() ? '' : '<!--',
                 'UI_XML_ANTHY_END': dict_anthy.found() ? '' : '-->',
                 'UI_XML_CANNA_START': dict_canna.found() ? '' : '<!--',
                 'UI_XML_CANNA_END': dict_canna.found() ? '' : '-->',
               },
               install_dir: pkgdatadir / 'helperdata')
