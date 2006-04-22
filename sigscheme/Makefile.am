SUBDIRS = doc m4 tools src lib test test-c bench
EXTRA_DIST = \
        TODO autogen.sh \
        compare-scm.sh runbench.sh runtest.sh runtest-tail-rec.sh

UIM_REPOSITORY = http://anonsvn.freedesktop.org/svn/uim
SSCM_REPOSITORY = $(UIM_REPOSITORY)/branches/r5rs/sigscheme
TAGS_REPOSITORY = $(UIM_REPOSITORY)/tags

.PHONY: FORCE
FORCE:

ChangeLog: FORCE
	svn export $(UIM_REPOSITORY)/trunk/tools/gnuify-changelog.pl \
	    $(top_srcdir)/tools/gnuify-changelog.pl
	svn log | $(top_srcdir)/tools/gnuify-changelog.pl > $@
	@test -s $@ && echo 'ChangeLog updated successfully.'

# To generate ChangeLog for a release properly, a source tree for official
# release must not be obtained by 'svn export' but 'svn co'.
#
# svn cp branches/r5rs/sigscheme tags/sigscheme-x.y.z
# cd /tmp
# svn co $(TAGS_REPOSITORY)/sigscheme-x.y.z
# cd sigscheme-x.y.z
# ./configure
# make dist
dist-hook:
	svn info \
	    | $(EGREP) -q "^Repository Root: .+/svn/uim/tags/sigscheme-" \
	    || (echo "error: not an official release. checkout tagged one"; test)
