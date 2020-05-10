.PHONY: all
all: bin dotfiles coc

.PHONY: bin
bin:
	if ! [ -d "$(HOME)/.bin" ]; then \
		mkdir "$(HOME)/.bin" ; \
	fi;
	for file in $(shell find $(CURDIR)/bin -type f -maxdepth 1); do \
		f=$$(basename $$file); \
		ln -sfn $$file $(HOME)/.bin/$$f; \
	done;

.PHONY: dotfiles
dotfiles:
	stow -t $(HOME) bash
	stow -t $(HOME) git
	stow -t $(HOME) tmux
	stow -t $(HOME) vim
	stow -t $(HOME) zsh


.PHONY: uninstall
uninstall:
	stow -t $(HOME) -D bash
	stow -t $(HOME) -D git
	stow -t $(HOME) -D tmux
	stow -t $(HOME) -D vim
	stow -t $(HOME) -D zsh

.PHONY: coc
coc:
	ln -fn coc-settings.json $(HOME)/.vim/coc-settings.json
