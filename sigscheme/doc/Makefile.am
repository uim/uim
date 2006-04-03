TEXIS = coreprocedures.texi \
        coresyntax.texi \
        features.texi \
        indexfn.texi \
        introduction.texi \
        references.texi \
        sscm-doc.texi

EXTRA_DIST = design.txt spec.txt style.txt test-c.txt $(TEXIS)

all: html

html: $(TEXIS)
	texi2html -split=section -subdir=sscm-doc sscm-doc.texi

pdf: $(TEXIS)
	texi2pdf sscm-doc.texi
