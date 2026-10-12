test_environment = environment({
  'LIBUIM_PLUGIN_LIB_DIR': meson.project_build_root() / 'uim',
  'LIBUIM_SCM_FILES': meson.project_source_root() / 'scm',
  'LIBUIM_SYSTEM_SCM_FILES': (meson.project_source_root() /
                              'subprojects' / 'sigscheme' / 'lib'),
})
# [test, should_fail]
foreach definition : [
  ['test-action.scm', false],
  ['test-custom.scm', false],
  ['test-fail.scm', true],
  ['test-im.scm', false],
  ['test-light-record.scm', false],
  ['test-template.scm', false],
  ['test-ustr.scm', false],
  ['test-wlos.scm', false],
  ['i18n/test-base.scm', false],
  ['i18n/test-language.scm', false],
  ['key/test-base.scm', false],
  ['key/test-predicate.scm', false],
  ['key/test-translator.scm', false],
  ['util/test-character-conversion.scm', false],
  ['util/test-character-predicate.scm', false],
  ['util/test-list.scm', false],
  ['util/test-misc.scm', false],
  ['util/test-r5rs.scm', false],
  ['util/test-record.scm', false],
  ['util/test-rk.scm', false],
  ['util/test-srfi.scm', false],
  ['util/test-string.scm', false],
  ['util/test-uim.scm', false],
]
  # uim-sh loads absolute paths only.
  test(definition[0],
       programs['uim-sh'],
       args: [meson.current_source_dir() / definition[0]],
       env: test_environment,
       should_fail: definition[1],
       suite: 'uim')
endforeach
