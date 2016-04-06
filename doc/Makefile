MARKDOWNS=$(sort $(wildcard *.md))
BIB=warden.bib
OUT=warden.pdf

all: $(OUT)

$(OUT): $(MARKDOWNS) $(BIB)
	pandoc --toc --smart --filter pandoc-citeproc --bibliography $(BIB) -f markdown+tex_math_dollars $(MARKDOWNS) -o $@

clean:
	rm -f $(OUT)

.PHONY: clean
