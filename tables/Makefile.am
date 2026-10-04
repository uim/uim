tablesdir = $(pkgdatadir)/tables

GENERATED_TABLES = wb86.table zm.table

dist_tables_DATA = $(GENERATED_TABLES)

TABLE_SOURCE_FILES = \
	source/README \
	source/wb86/COPYING \
	source/wb86/README \
	source/wb86/wubi-haifeng86.txt \
	source/zm/README \
	source/zm/zhengma.txt

EXTRA_DIST = \
	fetch-table-sources.rb \
	generate-table.rb \
	test/test-generate-table.rb \
	$(TABLE_SOURCE_FILES)

MAINTAINERCLEANFILES = $(GENERATED_TABLES)
CLEANFILES = $(GENERATED_TABLES:%=%.tmp)

if MAINTAINER_MODE

wb86.table: $(srcdir)/source/wb86/wubi-haifeng86.txt $(srcdir)/generate-table.rb
	$(AM_V_GEN)$(RUBY) $(srcdir)/generate-table.rb wb86 $< > $@.tmp && \
	mv $@.tmp $@

zm.table: $(srcdir)/source/zm/zhengma.txt $(srcdir)/generate-table.rb
	$(AM_V_GEN)$(RUBY) $(srcdir)/generate-table.rb zm $< > $@.tmp && \
	mv $@.tmp $@

endif

TEST_EXTENSIONS = .rb
RB_LOG_COMPILER = $(RUBY)

if HAVE_RUBY

TESTS = test/test-generate-table.rb

endif
