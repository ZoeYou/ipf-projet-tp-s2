CC=ocamlc
EXEC=main
all: $(EXEC)
	@./$(EXEC)

$(EXEC): ants.cmo main.cmo
	@$(CC) -o $@ $^

main.cmo: main.ml ants.cmo
	@$(CC) -c $<

ants.cmo: ants.ml qq
	@$(CC) -c $<

qq: ants.mli
	@$(CC) -c $<

.PHONY: clean mrproper

clean:
	@rm -rf *.cmo *.cmi

mrproper: clean
	@rm $(EXEC)