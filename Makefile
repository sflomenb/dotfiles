.PHONY: all
all: bin dotfiles

.PHONY: bin
bin:
	for file in $(shell find $(CURDIR)/bin -type f); do \
		f=$$(basename $$file); \
		ln -sfn $$file $(HOME)/bin/$$f; \
	done;

.PHONY: dotfiles
dotfiles:
	for file in $(shell find $(CURDIR) -maxdepth 1 -name ".*" -not -name '.git'); do \
		f=$$(basename $$file); \
		ln -sfn $$file $(HOME)/$$f; \
	done;
