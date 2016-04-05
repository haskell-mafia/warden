MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)

%.pdf: %.md
	pandoc -V geometry:margin=2cm -f markdown+tex_math_dollars $< -o $@

clean:
	rm -f $(PDFS)

.PHONY: clean
