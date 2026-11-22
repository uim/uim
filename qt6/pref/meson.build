qt6_pref_hdrs = [
  '../../qt4/pref/customwidgets.h',
  '../../qt4/pref/qt4.h',
  ]
qt6_pref_srcs = [
  '../../qt4/pref/customwidgets.cpp',
  '../../qt4/pref/qt4.cpp',
  '../../qt4/pref/keyeditformbase.h',
  '../../qt4/pref/keyeditformbase.cpp',
  '../../qt4/pref/olisteditformbase.h',
  '../../qt4/pref/olisteditformbase.cpp',
  ]
qt6_pref_mocs = qt6.compile_moc(
  headers: qt6_pref_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep)

executable('uim-pref-qt6',
  qt6_pref_hdrs,
  qt6_pref_srcs,
  qt6_pref_mocs,
  dependencies: qt6_dep,
  include_directories: uim_qt6_inc,
  install: true,
  link_with: [
    uim_qt6_libs,
    libuim_custom,
    libuim_counted_init,
    ],
  override_options: qt6_opts,
  )
