pandocflags = -s -S -t revealjs -V transition=fade --slide-level 2 --mathjax --parse-raw --include-in-header=custom.css --from markdown-implicit_figures


all: proposal.html

proposal.html: proposal.md
	pandoc $< -o $@ $(pandocflags)

proposal.md: proposal.Rmd
	Rscript -e "knitr::knit(\"$<\")"
