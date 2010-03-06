NAME=lambtex-quickref

$(NAME).pdf: $(NAME).tex Makefile
	pdflatex $<

hierarchy.pdf: ../lambtex-manual/hierarchy.svg
	inkscape -D -f $< -A $@

clean:
	rm -f $(NAME).aux $(NAME).log $(NAME).out $(NAME).pdf

