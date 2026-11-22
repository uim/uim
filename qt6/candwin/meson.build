qt6_candwin_hdrs = [
  '../../qt4/candwin/abstractcandidatewindow.h',
  '../../qt4/candwin/candidatetablewindow.h',
  '../../qt4/candwin/candidatewindow.h',
  '../../qt4/candwin/subwindow.h',
  '../../qt4/candwin/ximcandidatewindow.h',
  ]
qt6_candwin_srcs = [
  '../../qt4/candwin/abstractcandidatewindow.cpp',
  '../../qt4/candwin/candidatetablewindow.cpp',
  '../../qt4/candwin/candidatewindow.cpp',
  '../../qt4/candwin/qt4.cpp',
  '../../qt4/candwin/subwindow.cpp',
  '../../qt4/candwin/util.h',
  '../../qt4/candwin/util.cpp',
  '../../qt4/candwin/ximcandidatewindow.cpp',
  ]
qt6_candwin_mocs = qt6.compile_moc(
  headers: qt6_candwin_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep)

executable('uim-candwin-qt6',
  qt6_candwin_srcs,
  qt6_candwin_hdrs,
  qt6_candwin_mocs,
  dependencies: qt6_dep,
  include_directories: uim_qt6_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt6_libs,
  override_options: qt6_opts,
  )
