scmdir = $(datadir)/uim

MOSTLYCLEANFILES = $(GENERATED_SCM_FILES)
EXTRA_DIST = $(SCM_FILES)
scm_DATA = $(SCM_FILES) $(GENERATED_SCM_FILES)
GENERATED_SCM_FILES = installed-modules.scm loader.scm
SCM_FILES = plugin.scm im.scm im-custom.scm lazy-load.scm init.scm \
 default.scm \
 util.scm key.scm ustr.scm action.scm load-action.scm i18n.scm \
 ng-key.scm physical-key.scm event.scm evmap.scm evmap-csv.scm \
 event-translator.scm \
 key-custom.scm \
 uim-sh.scm custom.scm custom-rt.scm \
 manage-modules.scm \
 direct.scm \
 legacy-api-bridge.scm \
 ng-japanese.scm ng-japanese-romaji.scm japanese-romaji-custom.scm \
 ng-japanese-kana.scm japanese-nicola.scm ng-japanese-azik.scm \
 rk.scm \
 generic.scm generic-custom.scm generic-key-custom.scm \
 pyload.scm PY.scm pyunihan.scm pinyin-big5.scm \
 japanese.scm japanese-azik.scm japanese-kana.scm \
 anthy.scm anthy-custom.scm anthy-key-custom.scm \
 canna.scm canna-custom.scm canna-key-custom.scm \
 prime.scm prime-custom.scm prime-key-custom.scm \
 skk.scm skk-editor.scm skk-custom.scm skk-key-custom.scm \
 tcode.scm \
 tutcode.scm tutcode-key-custom.scm \
 hangul.scm hangul2.scm hangul3.scm romaja.scm \
 viqr.scm \
 ipa-x-sampa.scm \
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

module_names += "tcode" "tutcode" "hangul" "viqr" "ipa-x-sampa" "latin"

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
	| LIBUIM_VERBOSE=1 LIBUIM_VANILLA=1 LIBUIM_SCM_FILES=`(cd $(top_srcdir) && pwd)`/scm LIBUIM_PLUGIN_LIB_DIR=$(top_builddir)/uim/.libs \
	  $(top_builddir)/uim/uim-sh -B >$@

# TODO: resolve dependency to $(top_builddir)/uim/uim-sh in proper way
loader.scm: installed-modules.scm
	$(MAKE) -C $(top_builddir)/uim uim-sh && \
	$(ECHO) "(require \"manage-modules.scm\") \
	         (require (string-append (getenv \"PWD\") \
	                                 \"/installed-modules.scm\")) \
	         (generate-loader-scm)" \
	| LIBUIM_VERBOSE=1 LIBUIM_VANILLA=1 LIBUIM_SCM_FILES=`(cd $(top_srcdir) && pwd)`/scm LIBUIM_PLUGIN_LIB_DIR=$(top_builddir)/uim/.libs \
	  $(top_builddir)/uim/uim-sh -B >$@

# $(ECHO) $(ECHO_N) >$@
