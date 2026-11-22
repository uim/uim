qt6_switcher_source_dir = '..' / '..' / 'qt4' / 'switcher'
qt6_switcher_moc = qt6_module.compile_moc(
  headers: qt6_switcher_source_dir / 'qt4.h',
  dependencies: qt6,
  include_directories: qt6_include_directories)
executable('uim-im-switcher-qt6',
           qt6_switcher_source_dir / 'qt4.cpp',
           qt6_switcher_moc,
           dependencies: [qt6, intl],
           include_directories: qt6_include_directories,
           install: true,
           link_with: [qt6_link_with, libuim_custom],
           override_options: qt6_override_options)
