all:
	@ocamllex lexer.mll
	@ocamlopt -o lexer lexer.ml
	@./lexer input.txt
	@rm lexer
	@rm lexer.cmi
	@rm lexer.cmx
	@rm lexer.o
	@rm lexer.ml
