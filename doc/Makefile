DOC=warden
OUT=$(DOC).pdf

all: $(OUT)

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
