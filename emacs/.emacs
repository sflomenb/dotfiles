
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)
;(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(git-gutter:update-interval 1))
 ;; '(package-selected-packages
 ;;   '(xclip terraform-mode lsp-pyright dockerfile-mode ivy-rich counsel-projectile counsel ivy diminish flycheck-pycheckers typescript-mode flycheck yasnippet-classic-snippets yasnippet-snippets docker-tramp tramp emacsql-psql use-package lsp-ui company lsp-docker lsp-focus lsp-java lsp-origami origami vimish-fold ace-jump-mode python-pytest evil-surround evil-matchit which-key evil indent-guide yaml-mode git-gutter undohist magit gruvbox-theme free-keys lsp-mode ## json-mode expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#22aa22" :foreground "color-229"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#aa2222" :foreground "color-229")))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package yasnippet-snippets
  :config
  (use-package yasnippet-classic-snippets))

(use-package tramp
  :config
  (use-package docker-tramp))
(use-package emacsql-psql)
(use-package ace-jump-mode)
(use-package python-pytest)
(use-package indent-guide)
(use-package yaml-mode)
(use-package git-gutter
  :diminish
  :custom
  (git-gutter:window-width 2)
  :config
  (global-git-gutter-mode t)
  :bind (("C-x p" . 'git-gutter:previous-hunk)
   ("C-x n" . 'git-gutter:next-hunk)))
(use-package undohist
  :config
  (undohist-initialize))
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))
(use-package free-keys)
(use-package json-mode)

(use-package expand-region
    :bind ("C-^" . .er/expand-region))

(use-package projectile
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
  ("C-c M" . #'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))

(use-package ivy
  :demand
  :diminish
  :custom
  (ivy-height 30)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  (setq ivy-count-format "%d/%d ")

  ;; :bind (("C-c C-r" . #'ivy-resume)
  ;;        ("C-s"     . #'swiper)
  ;;        ("C-c s"   . #'swiper-thing-at-point))
  )

(use-package ivy-rich
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

(use-package counsel
  :bind (("C-c ;" . #'counsel-M-x)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-x f" . #'counsel-find-file)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-h h" . #'counsel-command-history)
         ("C-x C-f" . #'counsel-find-file)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :init
  (counsel-mode 1)

  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt (append completion-ignored-extensions '("node_modules/" ".log/"))))

  :diminish)

(use-package counsel-projectile
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project)))


;; (defun my/insert-line-before ()
;;   "Inserts a new line(s) above the line containing the cursor."
;;   (interactive)
;;   (save-excursion
;;     (move-beginning-of-line 1)
;;     (newline)))
;; 
;; (global-set-key (kbd "M-p")
;; 		'my/insert-line-before)
;; 
;; (defun my/insert-line-after ()
;;   "Inserts a new line(s) below the line containing the cursor."
;;   (interactive)
;;   (save-excursion
;;     (move-end-of-line 1)
;;     (newline)))
;; 
;; (global-set-key (kbd "M-P")
;; 		'my/insert-line-after)

; (global-linum-mode 1)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq next-line-add-newlines t)

; paren mode
(show-paren-mode 1)
(setq show-paren-delay 0)
;; auto insert closing bracket
(electric-pair-mode 1)

(use-package terraform-mode)

;; lsp-ui
(use-package company
  :diminish)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(use-package flycheck
  :init
  (global-set-key (kbd "C-c e n") 'flycheck-next-error)
  (global-set-key (kbd "C-c e p") 'flycheck-prev-error)
  (global-flycheck-mode))

;; Show indicators in the left margin
(setq flycheck-indication-mode 'left-margin)

;; Adjust margins and fringe widths…
(defun my/set-flycheck-margins ()
  (setq left-fringe-width 8 right-fringe-width 8
        left-margin-width 2 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

;; …every time Flycheck is activated in a new buffer
(add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)

;; flycheck-pycheckers
;; Allows multiple syntax checkers to run in parallel on Python code
;; Ideal use-case: pyflakes for syntax combined with mypy for typing
(use-package flycheck-pycheckers
  :after flycheck
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )
  (setq flycheck-pycheckers-checkers
    '(
      mypy3
      )
    )
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package dockerfile-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 (dockerfile-mode . lsp-deferred)
	 (terraform-mode . lsp-deferred)
	 ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config (setq lsp-ui-flycheck-live-reporting t))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package dap-mode)

(progn
(let ((map (if (boundp 'input-decode-map)
	    input-decode-map
function-key-map)))
  (define-key map "\e[1;P9"  (kbd "C-;"))
  (define-key map "\e[1;P10"  (kbd "C-."))))

;; company
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-;") #'company-indent-or-complete-common)

(defun setup-company-map ()
  (define-key company-active-map (kbd "C-j") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-k") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(add-hook 'company-mode-hook 'setup-company-map)

;; lsp modes

(use-package origami
  :config
  (use-package lsp-origami))
(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java"))
;;(require 'lsp-java)
;;(add-hook 'java-mode-hook #'lsp)
(use-package lsp-java
  :hook (java-mode . lsp-deferred))


(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package lsp-docker)
(use-package lsp-focus)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config (setq typescript-indent-level 2))


;; yasnippet
(use-package yasnippet
  :config (yas-global-mode 1)
  :diminish
  :bind (:map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)))
(use-package yasnippet-snippets)

;; Bind `C-.' to `yas-expand' when snippet expansion available (it
;; will still call `self-insert-command' otherwise).
(define-key yas-minor-mode-map (kbd "C-.") yas-maybe-expand)

;;(define-key yas-minor-mode-map (kbd "TAB") yas-next-field-or-maybe-expand)

;; Bind `C-c y' to `yas-expand' ONLY.
(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

(global-set-key (kbd "C-c C-f") 'focus-mode)


;; disable prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; from http://whattheemacsd.com

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(save-place-mode 1)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; newlines
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; turn off menu bar
(menu-bar-mode -1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq display-line-numbers-type 'relative)

(setq magit-diff-refine-hunk 'all)

(setq undohist-ignored-files (list "COMMIT_EDITMSG"))

(indent-guide-global-mode)

(use-package evil
  :config
  (evil-mode 1)
  ;; do not use evil in magit
  (add-to-list 'evil-buffer-regexps '("\\*magit:"))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)))

(use-package magit
  :config
  (setq magit-keep-region-overlay t))

(defun my/split-main-window (direction size)
  "Split the main window in the DIRECTION where DIRECTION is a symbol with
possible values of right, left, above or below and SIZE is the final size of the
windows, if the window is split horizontally (i.e. in DIRECTION below or above)
SIZE is assumed to be the target height otherwise SIZE is assumed to be the
target width"
  ;; (interactive
  ;;  (list
  ;;   (intern (read-string "Direction: "))
  ;;   (read-number "Size: ")))
  (let* ((new-window (split-window (frame-root-window) nil direction))
         (horizontal (member direction '(right left))))
    (save-excursion
      (select-window new-window)
      (enlarge-window (- size (if horizontal
                                  (window-width)
                                (window-height)))
                      horizontal))
    new-window))

;; (global-set-key (kbd "C-c s t")
;; 		(lambda () (interactive) (my/split-main-window 'above 5)))

(require 'whitespace)

(global-set-key (kbd "C-x w") 'whitespace-mode)

(require 'origami)

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)


(setq select-enable-clipboard nil)
(setq save-interprogram-paste-before-kill t)

(use-package xclip
  :config
  (xclip-mode 1))

(straight-use-package '(apheleia :host github :repo "raxod502/apheleia"))
(apheleia-global-mode +1)

(provide '.emacs)

;;; .emacs ends here

