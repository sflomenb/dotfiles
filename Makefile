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
	stow -t $(HOME) --ignore='test.el' emacs
	stow -t $(HOME) flake8
	stow -t $(HOME) git
	stow -t $(HOME) nvim
	stow -t $(HOME) psql
	stow -t $(HOME) tmux
	stow -t $(HOME) vim
	stow -t $(HOME) zsh


.PHONY: uninstall
uninstall:
	stow -t $(HOME) -D bash
	stow -t $(HOME) --ignore='test.el' -D emacs
	stow -t $(HOME) -D flake8
	stow -t $(HOME) -D git
	stow -t $(HOME) -D nvim
	stow -t $(HOME) -D psql
	stow -t $(HOME) -D tmux
	stow -t $(HOME) -D vim
	stow -t $(HOME) -D zsh

.PHONY: coc
coc:
	ln -fn $(shell pwd)/coc-settings.json $(HOME)/.vim/coc-settings.json
