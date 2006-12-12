BUILT_SOURCES = \
        config.h \
        config-old.h scmint.h global.h encoding.h encoding-dummy.h \
        sigscheme.h \
        storage-common.h storage-fatty.h storage-compact.h

config.h: $(top_builddir)/src/config.h
	rm -f $@
	$(LN_S) $< $@
config-old.h: $(top_srcdir)/src/config-old.h
	rm -f $@
	$(LN_S) $< $@
scmint.h: $(top_srcdir)/src/scmint.h
	rm -f $@
	$(LN_S) $< $@
global.h: $(top_srcdir)/src/global.h
	rm -f $@
	$(LN_S) $< $@
encoding.h: $(top_srcdir)/src/encoding.h
	rm -f $@
	$(LN_S) $< $@
encoding-dummy.h: $(top_srcdir)/src/encoding-dummy.h
	rm -f $@
	$(LN_S) $< $@
sigscheme.h: $(top_srcdir)/src/sigscheme.h
	rm -f $@
	$(LN_S) $< $@
storage-common.h: $(top_srcdir)/src/storage-common.h
	rm -f $@
	$(LN_S) $< $@
storage-fatty.h: $(top_srcdir)/src/storage-fatty.h
	rm -f $@
	$(LN_S) $< $@
storage-compact.h: $(top_srcdir)/src/storage-compact.h
	rm -f $@
	$(LN_S) $< $@

CLEANFILES = $(BUILT_SOURCES)
