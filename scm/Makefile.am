scmdir = $(datadir)/uim

MOSTLYCLEANFILES = $(GENERATED_SCM_FILES)
EXTRA_DIST = $(SCM_FILES)
scm_DATA = $(SCM_FILES) $(GENERATED_SCM_FILES)
GENERATED_SCM_FILES = installed-modules.scm loader.scm
SCM_FILES = plugin.scm im.scm im-custom.scm lazy-load.scm init.scm \
 default.scm \
 util.scm key.scm ustr.scm action.scm load-action.scm i18n.scm \
 uim-sh.scm custom.scm custom-rt.scm \
 manage-modules.scm \
 direct.scm \
 rk.scm \
 generic.scm generic-custom.scm generic-key-custom.scm \
 pyload.scm PY.scm pyunihan.scm pinyin-big5.scm \
 japanese.scm japanese-azik.scm japanese-kana.scm \
 anthy.scm anthy-custom.scm anthy-key-custom.scm \
 canna.scm canna-custom.scm canna-key-custom.scm \
 prime.scm prime-custom.scm prime-key-custom.scm \
 skk.scm skk-editor.scm skk-custom.scm skk-key-custom.scm \
 tcode.scm \
 tutcode.scm \
 hangul.scm hangul2.scm hangul3.scm romaja.scm \
 viqr.scm \
 ipa.scm \
 latin.scm \
 m17nlib.scm \
 spellcheck.scm spellcheck-custom.scm \
 zaurus.scm

if COMPAT_TABLE
SCM_FILES += hk.scm
endif


module_names = "pyload"

if ANTHY
  module_names += "anthy"
endif

if CANNA
  module_names += "canna"
endif

if PRIME
  module_names += "prime"
endif

if SKK
  module_names += "skk"
endif

module_names += "tcode" "tutcode" "hangul" "viqr" "ipa" "latin"

if M17NLIB
  module_names += "m17nlib"
endif

if SCIM
#  module_names += "scim"
endif

# TODO: resolve dependency to $(top_builddir)/uim/uim-sh in proper way
installed-modules.scm: $(SCM_FILES)
	$(MAKE) -C $(top_builddir)/uim uim-sh && \
	$(ECHO) "(define installed-im-module-list '(" '$(module_names)' ")) \
	      (require \"manage-modules.scm\") \
	      (generate-installed-modules-scm)" \
	| LIBUIM_VERBOSE=1 LIBUIM_VANILLA=1 LIBUIM_SCM_FILES=$(top_srcdir)/scm LIBUIM_PLUGIN_LIB_DIR=$(top_srcdir)/uim/.libs \
	  $(top_builddir)/uim/uim-sh -B >$@

# TODO: resolve dependency to $(top_builddir)/uim/uim-sh in proper way
loader.scm: installed-modules.scm
	$(MAKE) -C $(top_builddir)/uim uim-sh && \
	$(ECHO) "(require \"manage-modules.scm\") \
	      (generate-loader-scm)" \
	| LIBUIM_VERBOSE=1 LIBUIM_VANILLA=1 LIBUIM_SCM_FILES=$(top_srcdir)/scm LIBUIM_PLUGIN_LIB_DIR=$(top_srcdir)/uim/.libs \
	  $(top_builddir)/uim/uim-sh -B >$@

# $(ECHO) $(ECHO_N) >$@
