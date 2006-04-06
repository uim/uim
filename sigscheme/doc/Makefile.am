TXTS = design.txt spec.txt style.txt test-c.txt
TEXIS = coreprocedures.texi \
        coresyntax.texi \
        features.texi \
        indexfn.texi \
        introduction.texi \
        references.texi \
        sscm-doc.texi

# experimental
ASCIIDOC_HTMLS = design.html spec.html

BUILT_DIST = $(ASCIIDOC_HTMLS)
EXTRA_DIST = $(BUILT_DIST) TODO $(TXTS) $(TEXIS)

# xhtml11 backend is supported from AsciiDoc 7.0.0
.txt.html:
	if test -n "$(ASCIIDOC)"; then          \
	    $(ASCIIDOC) -b xhtml11 -n -o $@ $<; \
	fi

.PHONY: all html pdf
all: html

html: $(TEXIS) $(ASCIIDOC_HTMLS)
	texi2html -split=section -subdir=sscm-doc sscm-doc.texi

pdf: $(TEXIS)
	texi2pdf sscm-doc.texi

dist-hook:
	@test -n "$(ASCIIDOC)" || (echo 'asciidoc(1) not found: install AsciiDoc'; test)
