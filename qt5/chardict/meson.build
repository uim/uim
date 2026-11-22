qt5_chardict_hdrs = [
  '../../qt4/chardict/bushuviewwidget.h',
  '../../qt4/chardict/chargridview.h',
  '../../qt4/chardict/qt4.h',
  '../../qt4/chardict/unicodeviewwidget.h',
  ]
qt5_chardict_srcs = [
  '../../qt4/chardict/bushuviewwidget.cpp',
  '../../qt4/chardict/chargridview.cpp',
  '../../qt4/chardict/qt4.cpp',
  '../../qt4/chardict/unicodeviewwidget.cpp',
  ]
qt5_chardict_mocs = qt5.compile_moc(
  headers: qt5_chardict_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep)

executable('uim-chardict-qt5',
  qt5_chardict_srcs,
  qt5_chardict_hdrs,
  qt5_chardict_mocs,
  dependencies: [qt5_dep, x11],
  include_directories: uim_qt5_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt5_libs,
  )
