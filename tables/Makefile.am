tablesdir = $(pkgdatadir)/tables

SCMS = wb86.scm zm.scm
SCM_TABLES = wb86.table zm.table

NATIVE_TABLES = 

GENERATED_TABLES = $(SCM_TABLES)

TABLES = $(NATIVE_TABLES) $(GENERATED_TABLES)

dist_tables_DATA = $(TABLES)

MAINTAINERCLEANFILES = $(GENERATED_TABLES)

UIM_SH = $(top_builddir)/uim/uim-sh
UIM_SH_ENV = \
        LIBUIM_SYSTEM_SCM_FILES=$(abs_top_srcdir)/sigscheme/lib \
	LIBUIM_SCM_FILES=$(abs_top_srcdir)/scm \
	LIBUIM_PLUGIN_LIB_DIR=$(abs_top_builddir)/uim/.libs \
	UIM_DISABLE_NOTIFY=1

#if MAINTAINER_MODE
wb86.scm: $(top_srcdir)/scm/wb86.scm
	$(LN_S) $< $@

zm.scm: $(top_srcdir)/scm/zm.scm
	$(LN_S) $< $@

.scm.table:
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/sigscheme && \
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/replace && \
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/uim sigscheme-combined && \
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/uim uim-sh && \
	echo "(begin (load \"$<\") (for-each (lambda (key) (display (format \"~a ~W\n\" (apply string-append (caar key)) (cadr key)))) `basename $< .scm`-rule))" | $(UIM_SH_ENV) $(UIM_SH) -b | grep -v "^#<undef>" | LANG=C sort > $@
#endif

clean-genscm:
	rm -f $(SCMS)

clean-gentable:
	rm -f $(GENERATED_TABLES)
