test_environment = uim_build_tree_env + {'LIBUIM_VANILLA': '1'}
# [test, should_fail]
foreach definition : [
  ['test-action.scm', false],
  ['test-anthy.scm', false],
  ['test-custom-rt.scm', false],
  ['test-custom.scm', false],
  ['test-fail.scm', true],
  ['test-hangul.scm', false],
  ['test-iconv.scm', false],
  ['test-im.scm', false],
  ['test-lazy-load.scm', false],
  ['test-light-record.scm', false],
  ['test-plugin.scm', false],
  ['test-skk-custom.scm', false],
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

# Input methods driven through libuim by Ruby. Fake servers stand in for
# the services they use, so they run without the network.
im_driver = executable('im-driver',
                       'im' / 'im-driver.c',
                       include_directories: uim_include_directories,
                       link_with: [libuim_scm, libuim])
# Ruby is required only when the tests are enabled explicitly.
ruby = find_program('ruby', required: get_option('tests').enabled())
if ruby.found()
  test('im',
       ruby,
       args: [files('im' / 'run.rb')],
       depends: im_driver,
       env: uim_build_tree_env + {
         'IM_DRIVER': im_driver.full_path(),
         # ~/.uim and the user's customizations stay out of the test.
         'LIBUIM_VANILLA': '2',
       },
       # Each test starts im-driver. It takes about 1 second on CI.
       timeout: 300,
       suite: 'uim')
endif
