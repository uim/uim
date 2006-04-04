TEXIS = coreprocedures.texi \
        coresyntax.texi \
        features.texi \
        indexfn.texi \
        introduction.texi \
        references.texi \
        sscm-doc.texi

# experimental
ASCIIDOC_HTMLS = design.html spec.html

EXTRA_DIST = \
        design.txt spec.txt style.txt test-c.txt $(TEXIS) $(ASCIIDOC_HTMLS)

.txt.html:
	asciidoc -n -o $@ $<

.PHONY: all html pdf
all: html

html: $(TEXIS) $(ASCIIDOC_HTMLS)
	texi2html -split=section -subdir=sscm-doc sscm-doc.texi

pdf: $(TEXIS)
	texi2pdf sscm-doc.texi
