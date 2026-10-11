test_environment = environment({
  'LIBUIM_PLUGIN_LIB_DIR': meson.project_build_root() / 'uim',
  'LIBUIM_SCM_FILES': meson.project_source_root() / 'scm',
  'LIBUIM_SYSTEM_SCM_FILES': (meson.project_source_root() /
                              'subprojects' / 'sigscheme' / 'lib'),
})
# [test, should_fail]
foreach definition : [
  ['test-fail.scm', true],
  ['test-light-record.scm', false],
  ['test-template.scm', false],
  ['test-wlos.scm', false],
  ['util/test-list.scm', false],
]
  # uim-sh loads absolute paths only.
  test(definition[0],
       programs['uim-sh'],
       args: [meson.current_source_dir() / definition[0]],
       env: test_environment,
       should_fail: definition[1],
       suite: 'uim')
endforeach
