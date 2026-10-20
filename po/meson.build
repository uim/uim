i18n = import('i18n')

# For the .mo files. Don't use the uim-pot and uim-update-po targets
# that this defines. Use the pot and update-po targets below, which
# share update-po.sh with Autotools, instead.
i18n.gettext(meson.project_name(), install: true)

xgettext = find_program('xgettext', required: false)
msgmerge = find_program('msgmerge', required: false)
msgattrib = find_program('msgattrib', required: false)
if xgettext.found() and msgmerge.found() and msgattrib.found()
  foreach target : ['pot', 'update-po']
    run_target(target,
               command: [
                 find_program('update-po.sh'),
                 target,
                 meson.project_source_root(),
                 meson.current_build_dir(),
                 meson.project_version(),
                 xgettext,
                 msgmerge,
                 msgattrib,
               ])
  endforeach
endif
