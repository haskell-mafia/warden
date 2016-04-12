DOC=warden
OUT=$(DOC).pdf

all: $(OUT)

# Duplication intentional here, the latex-bibtex dance requires multiple steps.
$(OUT): $(DOC).tex $(DOC).bib
	pdflatex $(DOC).tex
	bibtex $(DOC).aux
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

clean:
	rm -f $(OUT)
	rm -f *.aux
	rm -f *.log

.PHONY: clean
