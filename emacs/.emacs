
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
 '(evil-ex-search-persistent-highlight nil)
 '(evil-search-module 'evil-search)
 '(evil-want-C-w-in-emacs-state t)
 '(magit-revision-insert-related-refs 'mixed))
 ;; '(package-selected-packages
 ;;   '(xclip terraform-mode lsp-pyright dockerfile-mode ivy-rich counsel-projectile counsel ivy diminish flycheck-pycheckers typescript-mode flycheck yasnippet-classic-snippets yasnippet-snippets docker-tramp tramp emacsql-psql use-package lsp-ui company lsp-docker lsp-focus lsp-java lsp-origami origami vimish-fold ace-jump-mode python-pytest evil-surround evil-matchit which-key evil indent-guide yaml-mode git-gutter undohist magit gruvbox-theme free-keys lsp-mode ## json-mode expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#22aa22" :foreground "color-229"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#aa2222" :foreground "color-229"))))
 '(ediff-current-diff-A ((t (:extend t :background "red" :foreground "brightwhite"))))
 '(ediff-current-diff-Ancestor ((t (:extend t :background "#ccc6d1" :foreground "black"))))
 '(ediff-current-diff-B ((t (:extend t :background "green" :foreground "brightwhite"))))
 '(ediff-current-diff-C ((t (:extend t :background "blue" :foreground "brightwhite"))))
 '(ediff-fine-diff-A ((t (:foreground "color-240"))))
 '(ediff-fine-diff-Ancestor ((t (:background "#b6b0d6" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:foreground "color-240"))))
 '(ediff-fine-diff-C ((t (:background "color-45" :foreground "color-240")))))

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

(use-package tramp)
(use-package docker-tramp
  :after (tramp))
(use-package emacsql-psql)
(use-package python-pytest)
(use-package indent-guide)
(use-package yaml-mode)

(straight-use-package '(diff-hl :host github :repo "dgutov/diff-hl"))
(global-diff-hl-mode)
(diff-hl-margin-mode)

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
  :bind (("C-c M" . #'projectile-compile-project))
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

  :bind (("C-c k" . #'counsel-ag)
	 ("C-c C-o" . #'ivy-occur))
	 ;; ("C-c C-r" . #'ivy-resume)
         ;; ("C-s"     . #'swiper)
         ;; ("C-c s"   . #'swiper-thing-at-point))
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
         ("C-x b" . #'counsel-ibuffer)
	 ("C-c k" . #'counsel-projectile-ag)
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
  :bind (("C-c F" . #'counsel-projectile-switch-project)))


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
(setq company-minimum-prefix-length 1)

(use-package flycheck
  :init
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

(use-package go-mode)

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
	 (json-mode . lsp-deferred)
	 (yaml-mode . lsp-deferred)
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
(global-set-key (kbd "C-;") #'company-complete-common-or-cycle)

(defun setup-company-map ()
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1))))

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
(global-set-key (kbd "C-c C-f") 'focus-mode)
(add-hook 'focus-mode-hook #'lsp-focus-mode)

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
(use-package yasnippet-snippets
  :after (yasnippet))


;; Bind `C-.' to `yas-expand' when snippet expansion available (it
;; will still call `self-insert-command' otherwise).
(define-key yas-minor-mode-map (kbd "C-.") yas-maybe-expand)

;;(define-key yas-minor-mode-map (kbd "TAB") yas-next-field-or-maybe-expand)

;; Bind `C-c y' to `yas-expand' ONLY.
(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)



;; disable prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)

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

(setq display-line-numbers-type 'visual)

(defun my/toggle-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (cond ((string= display-line-numbers "visual") (setq display-line-numbers t))
	((string= display-line-numbers "t") (setq display-line-numbers 'visual))))

(global-set-key (kbd "C-c t") 'my/toggle-relative-line-numbers)

(defun my/turn-on-absolute-numbers-for-window (win)
  (with-selected-window win
    (if display-line-numbers
	(setq display-line-numbers t))))

(defun my/switch-relative-numbers-off-previous-window (arg)
  "Switch relative numbers on for current window and off for old window."
  (if (or
       ;; old window was closed
       (not (window-buffer (old-selected-window)))
       ;; going to a window with the same file
       (not (string= (buffer-file-name) (buffer-file-name (window-buffer (old-selected-window))))))
      (progn
	(if display-line-numbers
	    (setq display-line-numbers 'visual))

	(unless (minibufferp)
	  (my/turn-on-absolute-numbers-for-window (old-selected-window))))))

(setq window-selection-change-functions '(my/switch-relative-numbers-off-previous-window))

(defun my/turn-relative-numbers-off-other-windows ()
  (interactive)
  (let ((current-window (selected-window)))
    (walk-windows
     (lambda (win)
       (unless (eq win current-window)
	 (my/turn-on-absolute-numbers-for-window win)))
     nil 'visible)))

(global-set-key (kbd "C-c w") 'ace-window)

(setq magit-diff-refine-hunk 'all)

(setq undohist-ignored-files (list "COMMIT_EDITMSG"))

(indent-guide-global-mode)

(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)

(use-package evil
  :config
  (evil-mode 1)
  ;; do not use evil in magit
  (add-to-list 'evil-buffer-regexps '("\\*magit:")))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") #'xref-goto-xref)
(define-key evil-normal-state-map (kbd "z j") 'origami-next-fold)
(define-key evil-normal-state-map (kbd "z k") 'origami-previous-fold)

(defun my/visual-star-search (beg end forward)
  "Search in direction FORWARD for visual selection between BEG and END."
  (when (evil-visual-state-p)
    (let* ((thing-to-search (buffer-substring-no-properties (region-beginning) (region-end))))
      (cl-flet ((func (&rest r) thing-to-search))
	(evil-exit-visual-state)
	(advice-add 'evil-find-thing :override #'func)
	(evil-ex-start-word-search t (if forward 'forward 'backward) 1)
	(advice-remove 'evil-find-thing #'func)))))

(defun my/visual-star-search-forward (beg end)
  "Search forward for visual selection between BEG and END."
  (interactive "r")
  (my/visual-star-search beg end t))

(defun my/visual-star-search-backward (beg end)
  "Search backward for visual selection between BEG and END."
  (interactive "r")
  (my/visual-star-search beg end nil))

(evil-global-set-key 'visual (kbd "*") 'my/visual-star-search-forward)
(evil-global-set-key 'visual (kbd "#") 'my/visual-star-search-backward)

(evil-global-set-key 'normal (kbd "M-H") 'evil-ex-nohighlight)

(define-key evil-normal-state-map (kbd "] g") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "[ g") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)

(use-package magit
  :bind (("C-c g" . 'magit-file-dispatch))
  :config
  (setq magit-keep-region-overlay t))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "C-u C-j") 'magit-diff-visit-worktree-file-other-window))

;; https://emacs.stackexchange.com/a/13831
(defun magit-diff-mbase (&optional args)
  "Show diff of $(git diff merge-base master HEAD) to working tree."
  (interactive (list (magit-diff-arguments)))
  (magit-diff-working-tree
   (magit-git-string "merge-base" "origin/master" "HEAD") args))

(transient-append-suffix 'magit-diff "w" '("m" "Diff merge-base master" magit-diff-mbase))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(require 'whitespace)

(global-set-key (kbd "C-x w") 'whitespace-mode)

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)

(global-set-key (kbd "C-c C-n") 'next-buffer)
(global-set-key (kbd "C-c C-p") 'previous-buffer)


(setq select-enable-clipboard nil)
(setq save-interprogram-paste-before-kill t)

(use-package xclip
  :config
  (xclip-mode 1))

(straight-use-package '(apheleia :host github :repo "raxod502/apheleia"))
(apheleia-global-mode +1)

(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")

(straight-use-package '(jest :host github :repo "Emiller88/emacs-jest"))
(add-hook 'js2-mode-hook 'jest-minor-mode)
(add-hook 'js-mode-hook 'jest-minor-mode)
(add-hook 'typescript-mode-hook 'jest-minor-mode)

(global-set-key (kbd "C-x t t") 'treemacs)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(straight-use-package '(evil-org :host github :repo "Somelauw/evil-org-mode"))
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
(add-hook 'org-mode-hook 'flyspell-mode)

(straight-use-package '(fzf :host github :repo "bling/fzf.el"))
(global-set-key (kbd "C-c f") 'fzf)

(setq js-log '(("default" .
		(("call" .  "console.log(\"\")")
		 ("seperator" . ",")
		 ("function-to-call" . "my/json-stringify")
		 ("eol-char" . ";")))))

(setq logging-alist
      '(("python-mode" .
	 (("has-logging" . ("import logging" "logger = "))
	  ("logging" .
	   (("call" .  "logger.info(\"\")")
	    ("seperator" . " %")
	    ("placeholder" . "%s")))
	  ("default" .
	   (("call" .  "print(\"\")")
	    ("seperator" . ",")))))
	("javascript-mode" . js-log)
	("js-mode" . js-log)
	("js2-mode" . js-log)
	("typescript-mode" . js-log)
	("go-mode" .
	 (("has-logging" . ("log"))
	  ("logging" .
	   (("call" .  "log.Printf(\"\")")
	    ("seperator" . ",")
	    ("placeholder" . "%#v\\n")))
	  ("default" .
	   (("call" .  "fmt.Printf(\"\")")
	    ("seperator" . ",")
	    ("placeholder" . "%#v\\n")))))))

(defun is-logging (list-of-text-to-search)
  (if (= (length list-of-text-to-search) 0) t
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(and
	 (search-forward (car list-of-text-to-search) nil t)
	 (is-logging (cdr list-of-text-to-search)))))))

(defun my/alist-get-symbol (key alist &optional default)
  (let ((ret-val (alist-get key alist default nil 'equal)))
    (if (symbolp ret-val) (symbol-value ret-val) ret-val)))

(defun log-word-at-point (&optional beg end)
  (interactive)
  (save-excursion
    (save-match-data
      (let ((log-info (my/alist-get-symbol (format "%s" major-mode) logging-alist)))
	(let ((log-info-from-alist
	       (if (and
		    (my/alist-get-symbol "has-logging" log-info)
		    (is-logging (my/alist-get-symbol "has-logging" log-info)))
		   (my/alist-get-symbol "logging" log-info)
		 (my/alist-get-symbol "default" log-info))))

	  (let ((current-word
		 (if (region-active-p)
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (thing-at-point 'symbol 'no-properties))))
	    (when-let* ((eol-char (my/alist-get-symbol "eol-char" log-info-from-alist))) (search-forward eol-char nil t))
	    (move-end-of-line nil)
	    (newline-and-indent)
	    (insert (my/alist-get-symbol "call" log-info-from-alist))
	    (move-end-of-line nil)
	    (search-backward "\"" nil t)
	    (insert (file-name-nondirectory (buffer-file-name)) ":" (int-to-string (line-number-at-pos)) " - " current-word ": " (my/alist-get-symbol "placeholder" log-info-from-alist ""))
	    (move-end-of-line nil)
	    (backward-char)
	    (insert (my/alist-get-symbol "seperator" log-info-from-alist) " " current-word)
	    (when-let* ((fun-to-call (my/alist-get-symbol "function-to-call" log-info-from-alist)))
	      (funcall (intern fun-to-call)))
	    (move-end-of-line nil)
	    (insert (my/alist-get-symbol "eol-char" log-info-from-alist ""))))))))

(defun setup-logging (map-to-add)
  (define-key map-to-add (kbd "C-c g") 'log-word-at-point))

(add-hook 'python-mode-hook (lambda () (setup-logging python-mode-map)))
(add-hook 'js-mode-hook (lambda () (setup-logging js-mode-map)))
(add-hook 'js2-mode-hook (lambda () (setup-logging js2-mode-map)))
(add-hook 'typescript-mode-hook (lambda () (setup-logging typescript-mode-map)))
(add-hook 'go-mode-hook (lambda () (setup-logging go-mode-map)))

(defun my/json-stringify ()
  (interactive)
  (save-excursion
    (save-match-data
      (let ((thing (thing-at-point 'symbol 'no-properties))
	    (bounds (bounds-of-thing-at-point 'symbol)))
	(if bounds
	    (progn
	      (kill-region (car bounds) (cdr bounds))
	      (insert "JSON.stringify(" thing ")")))))))

(defun my/desktop-save (session-name)
  (interactive "sSession name: ")
  (setq dir-name (concat "~/.emacs.d/desktops/" session-name "/"))
  (unless (file-directory-p dir-name) (make-directory dir-name))
  (setq desktop-path (list dir-name))
  (desktop-save dir-name)
  (desktop-save-mode 1))

(defun my/desktop-read ()
  (interactive)
  (my/save-and-release-desktop)
  (setq dir-name (read-directory-name "Directory: " "~/.emacs.d/desktops/"))
  (setq desktop-path (list dir-name))
  (desktop-read dir-name)
  (desktop-save-mode 1)
  (my/turn-relative-numbers-off-other-windows))

(defun my/save-and-release-desktop ()
  (interactive)
  "Save and release desktop which removes lock file."
  (if (and desktop-save-mode desktop-path dir-name)
      ;; save existing desktop
      (progn
	(desktop-save dir-name t)
	(desktop-save-mode 0)
	(message "Saved desktop."))
    (when (called-interactively-p 'any)
      (user-error "No current desktop to save"))))

;; https://emacs.stackexchange.com/a/45829
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
 (lambda ()
   (frameset-restore
    desktop-saved-frameset
    :reuse-frames (eq desktop-restore-reuses-frames t)
    :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
    :force-display desktop-restore-in-current-display
    :force-onscreen desktop-restore-forces-onscreen)))

(straight-use-package '(rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters"))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide '.emacs)

;;; .emacs ends here

