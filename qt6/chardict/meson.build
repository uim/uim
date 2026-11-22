qt6_chardict_hdrs = [
  '../../qt4/chardict/bushuviewwidget.h',
  '../../qt4/chardict/chargridview.h',
  '../../qt4/chardict/qt4.h',
  '../../qt4/chardict/unicodeviewwidget.h',
  ]
qt6_chardict_srcs = [
  '../../qt4/chardict/bushuviewwidget.cpp',
  '../../qt4/chardict/chargridview.cpp',
  '../../qt4/chardict/qt4.cpp',
  '../../qt4/chardict/unicodeviewwidget.cpp',
  ]
qt6_chardict_mocs = qt6.compile_moc(
  headers: qt6_chardict_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep)

executable('uim-chardict-qt6',
  qt6_chardict_srcs,
  qt6_chardict_hdrs,
  qt6_chardict_mocs,
  dependencies: [qt6_dep, x11],
  include_directories: uim_qt6_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt6_libs,
  override_options: qt6_opts,
  )
