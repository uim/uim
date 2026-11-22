qt6_candwin_source_dir = '..' / '..' / 'qt4' / 'candwin'
# The headers that have Q_OBJECT.
qt6_candwin_moc = qt6_module.compile_moc(
  headers: [
    qt6_candwin_source_dir / 'abstractcandidatewindow.h',
    qt6_candwin_source_dir / 'candidatetablewindow.h',
    qt6_candwin_source_dir / 'candidatewindow.h',
    qt6_candwin_source_dir / 'subwindow.h',
    qt6_candwin_source_dir / 'ximcandidatewindow.h',
  ],
  dependencies: qt6,
  include_directories: qt6_include_directories)
# The immodule runs this to show candidates.
executable('uim-candwin-qt6',
           qt6_candwin_source_dir / 'abstractcandidatewindow.cpp',
           qt6_candwin_source_dir / 'candidatetablewindow.cpp',
           qt6_candwin_source_dir / 'candidatewindow.cpp',
           qt6_candwin_source_dir / 'qt4.cpp',
           qt6_candwin_source_dir / 'subwindow.cpp',
           qt6_candwin_source_dir / 'util.cpp',
           qt6_candwin_source_dir / 'ximcandidatewindow.cpp',
           qt6_candwin_moc,
           dependencies: [qt6, intl],
           include_directories: qt6_include_directories,
           install: true,
           install_dir: libexecdir,
           link_with: qt6_link_with,
           override_options: qt6_override_options)
