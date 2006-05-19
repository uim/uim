scmdir = $(pkgdatadir)

MOSTLYCLEANFILES = $(GENERATED_SCM_FILES)
dist_scm_DATA = $(SCM_FILES)
scm_DATA = $(GENERATED_SCM_FILES)
GENERATED_SCM_FILES = installed-modules.scm loader.scm
SCM_FILES = plugin.scm im.scm im-custom.scm lazy-load.scm init.scm \
 im-switcher.scm \
 default.scm \
 util.scm key.scm ustr.scm action.scm load-action.scm i18n.scm \
 uim-sh.scm uim-db.scm custom.scm custom-rt.scm \
 direct.scm \
 rk.scm \
 generic.scm generic-custom.scm generic-key-custom.scm \
 pyload.scm py.scm pyunihan.scm pinyin-big5.scm \
 japanese.scm japanese-azik.scm japanese-kana.scm \
 anthy.scm anthy-custom.scm anthy-key-custom.scm \
 canna.scm canna-custom.scm canna-key-custom.scm \
 prime.scm prime-custom.scm prime-key-custom.scm \
 skk.scm skk-editor.scm skk-custom.scm skk-key-custom.scm skk-dialog.scm \
 mana.scm mana-custom.scm mana-key-custom.scm \
 tcode.scm \
 tutcode.scm tutcode-key-custom.scm \
 hangul.scm hangul2.scm hangul3.scm romaja.scm \
 byeoru.scm byeoru-dic.scm byeoru-symbols.scm \
 byeoru-custom.scm byeoru-key-custom.scm \
 viqr.scm \
 ipa-x-sampa.scm \
 latin.scm \
 m17nlib.scm \
 spellcheck.scm spellcheck-custom.scm \
 zaurus.scm \
 scim.scm \
 uim-module-manager.scm

module_names = "pyload"
if ANTHY
  module_names += "anthy"
endif
if CANNA
  module_names += "canna"
endif
if MANA
  module_names += "mana"
endif
if PRIME
  module_names += "prime"
endif
if SKK
  module_names += "skk"
endif
module_names += "tcode" "tutcode" "byeoru" "hangul" "viqr" "ipa-x-sampa" \
		"latin"
if M17NLIB
  module_names += "m17nlib"
endif
if SCIM
  module_names += "scim"
endif

UIM_MODULE_MANAGER = $(top_builddir)/uim/uim-module-manager
UIM_MODULE_MANAGER_ENV = \
        LIBUIM_SCM_FILES=$(top_srcdir)/scm \
        LIBUIM_PLUGIN_LIB_DIR=$(top_builddir)/uim/.libs

installed-modules.scm loader.scm: $(SCM_FILES)
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/uim uim-module-manager && \
	$(UIM_MODULE_MANAGER_ENV) $(UIM_MODULE_MANAGER) \
	    --path $(top_builddir)/scm --register $(module_names)
