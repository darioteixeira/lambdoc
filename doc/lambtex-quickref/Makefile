NAME=lambtex-quickref

$(NAME).pdf: $(NAME).tex Makefile
	pdflatex $<

clean:
	rm -f $(NAME).aux $(NAME).log $(NAME).out $(NAME).pdf

