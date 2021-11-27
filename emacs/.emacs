
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
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#aa2222" :foreground "color-229")))))

;; https://blog.d46.us/advanced-emacs-startup/
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

(setq straight-use-package-by-default t)
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

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
	    ;; Make gc pauses faster by decreasing the threshold.
	    (setq gc-cons-threshold (* 2 1000 1000))))

(use-package diminish)

(autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
(when (executable-find "docker")
  (use-package docker-tramp))
(when (or (executable-find "psql")  (executable-find "docker"))
  (use-package emacsql-psql))
(when
    (executable-find "pytest")
  (use-package python-pytest
    :bind (:map python-mode-map
		("C-c t" . 'python-pytest-dispatch))))

(use-package indent-guide
  :config
  (indent-guide-global-mode))
(use-package highlight-indent-guides
  :disabled
  :hook ((prog-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))
(use-package yaml-mode)

(use-package diff-hl
  :straight (:host github :repo "dgutov/diff-hl")
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package undohist
  :config
  (undohist-initialize)
  (setq undohist-ignored-files (list "COMMIT_EDITMSG")))

(use-package gruvbox-theme
  :config
  (if (= (display-color-cells) 16777216)
      (load-theme 'gruvbox-light-medium t)
    (load-theme 'gruvbox t)))

;; auto switch theme by time of day
;; https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
(defvar current-theme nil)

(defun synchronize-theme ()
  "Set theme based on time of day."
  (let* ((hour (string-to-number (substring (current-time-string) 11 13)))
	 (is-darwin (string= system-type "darwin"))
	 (light-dark-script "~/light-dark.scpt")
	 (theme-to-change-to (if (and is-darwin (file-exists-p light-dark-script))
				 (let ((result (shell-command-to-string (format "osascript %s" light-dark-script)))
				       (case-fold-search t))
				   (if (string-match-p "light" result)
				       'gruvbox-light-medium 'gruvbox))
			       (if (member hour (number-sequence 6 19))
				   'gruvbox-light-medium 'gruvbox))))
    (when (not (string= theme-to-change-to current-theme))
      (setq current-theme theme-to-change-to)
      (load-theme theme-to-change-to t))
    ;; Change iTerm2 theme if on Mac
    (when is-darwin
      (let ((iterm2-python-path "~/Library/ApplicationSupport/iTerm2/iterm2env/versions/3.8.6/bin/python3")
	    (toggle-script-path "~/Library/ApplicationSupport/iTerm2/Scripts/iterm2-light-dark-toggle.py"))
	(when (seq-every-p #'file-exists-p (list iterm2-python-path toggle-script-path))
	  (shell-command (format "%s %s" iterm2-python-path toggle-script-path)))))))

(when (= (display-color-cells) 16777216)
  (progn
    (synchronize-theme)
    (run-with-timer 0 300 'synchronize-theme)))

(use-package free-keys)
(use-package json-mode)

(use-package projectile
  :diminish
  :bind (("C-c M" . #'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))

(defun visit-buffer-or-file (buffer-or-file)
  (cond ((string= (type-of buffer-or-file) "cons")
	 (counsel-ibuffer-visit-buffer buffer-or-file))
	((string-match-p (regexp-quote ":") buffer-or-file)
	 (counsel-git-grep-action buffer-or-file))
	(t (let ((default-directory counsel--fzf-dir))
	     (find-file buffer-or-file)))))

(defun split-left ()
  (split-window-right))

(defun split-right ()
  (split-window-right)
  (other-window 1))

(defun split-above ()
  (split-window-below))

(defun split-below ()
  (split-window-below)
  (other-window 1))

(defun split-far-left ()
  (split-window-below)
  (evil-window-move-far-left))

(defun split-far-right ()
  (split-window-below)
  (evil-window-move-far-right))

(defun split-top ()
  (split-window-below)
  (evil-window-move-very-top))

(defun split-bottom ()
  (split-window-below)
  (evil-window-move-very-bottom))

(defun split-new-tab ()
  (tab-new))

(defun split-other-window ()
  (let ((new-window (display-buffer (current-buffer) '((display-buffer-pop-up-window) . ((inhibit-same-window . t))))))
    (if new-window (select-window new-window)
      (other-window 1))))

(defun split-in-direction (&rest r)
  "Split window in evil direction, potentially call function and args within R."
  (let* ((key (read-key-sequence "Direction or enter: ")))
    (cond
     ((string= key "j") (split-below))
     ((string= key "k") (split-above))
     ((string= key "h") (split-left))
     ((string= key "l") (split-right))
     ((string= key "J") (split-bottom))
     ((string= key "K") (split-top))
     ((string= key "H") (split-far-left))
     ((string= key "L") (split-far-right))
     ((string= key "t") (split-new-tab))
     ((string= key "O") (split-other-window))
     ((functionp (car r)) (apply (car r) (cdr r))))))

(advice-add 'xref-find-definitions :before #'split-in-direction)
(advice-add 'lsp-find-definition   :before #'split-in-direction)
(advice-add 'evil-window-split     :around #'split-in-direction)
(advice-add 'dired                 :before #'split-in-direction)

(defun my/ibuffer ()
  "Call `ibuffer' after `split-in-direction'.
This is used because `ibuffer' is called during counsel-ibuffer."
  (interactive)
  (split-in-direction)
  (ibuffer))

(global-set-key (kbd "C-c C-u") #'my/ibuffer)

(defun find-file-left (filename)
  (split-left)
  (visit-buffer-or-file filename))

(defun find-file-right (filename)
  (split-right)
  (visit-buffer-or-file filename))

(defun find-file-above (filename)
  (split-above)
  (visit-buffer-or-file filename))

(defun find-file-below (filename)
  (split-below)
  (visit-buffer-or-file filename))

(defun find-file-far-left (filename)
  (split-far-left)
  (visit-buffer-or-file filename))

(defun find-file-far-right (filename)
  (split-far-right)
  (visit-buffer-or-file filename))

(defun find-file-top (filename)
  (split-top)
  (visit-buffer-or-file filename))

(defun find-file-bottom (filename)
  (split-bottom)
  (visit-buffer-or-file filename))

(defun find-file-new-tab (filename)
  (tab-new)
  (visit-buffer-or-file filename))

(defun find-file-other-window (filename)
  (split-other-window)
  (visit-buffer-or-file filename))


;; https://github.com/abo-abo/swiper/issues/1068
(defun my/ivy-with-thing-at-point (cmd &optional dir)
  (let ((ivy-initial-inputs-alist
	 (list
	  (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd nil dir)))

(defun my/counsel-ag (&optional dir)
  (interactive "D")
  (my/ivy-with-thing-at-point
   'counsel-ag
   (or dir (file-name-directory (buffer-file-name)))))

(defun my/counsel-projectile-ag ()
  (interactive)
  (my/ivy-with-thing-at-point
   'counsel-projectile-ag))

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
  (ivy-set-actions
   t
   '(("j" find-file-below "open below")
     ("k" find-file-above "open above")
     ("h" find-file-left "open left")
     ("l" find-file-right "open right")
     ("J" find-file-bottom "open bottom")
     ("K" find-file-top "open top")
     ("H" find-file-far-left "open far-left")
     ("L" find-file-far-right "open far-right")
     ("t" find-file-new-tab "open in a new tab")
     ("O" find-file-other-window "open in a new tab")))

  :bind (("C-c K" . #'my/counsel-ag)
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
  (ivy-rich-mode)
  :after (ivy counsel))

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
	 ("C-c f" . #'counsel-fzf)
         ("C-x C-f" . #'counsel-find-file)
	 ("C-x d" . #'counsel-dired)
	 ("C-c d" . #'counsel-dired-jump)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :init
  (counsel-mode 1)

  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt (append completion-ignored-extensions '("node_modules/" ".log/"))))

  :diminish)

(use-package counsel-projectile
  :bind (("C-c F" . #'counsel-projectile-switch-project)))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq next-line-add-newlines t)

; paren mode
(show-paren-mode 1)
(setq show-paren-delay 0)
;; auto insert closing bracket
(electric-pair-mode 1)

(when
    (executable-find "terraform")
  (use-package terraform-mode))

(defun setup-company-map ()
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(use-package company
  :diminish
  :init
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  :hook ((company-mode . setup-company-map))
  :bind (("C-;" . company-complete)))


;; Adjust margins and fringe widths…
(defun my/set-flycheck-margins ()
  (setq left-fringe-width 8 right-fringe-width 8
        left-margin-width 2 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  ;; Show indicators in the left margin
  (setq flycheck-indication-mode 'left-margin)
  (setq sentence-end-double-space nil)
  ;; …every time Flycheck is activated in a new buffer
  :hook (flycheck-mode . my/set-flycheck-margins))

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

(when
    (executable-find "docker")
  (use-package dockerfile-mode))

(when
    (executable-find "go")
  (use-package go-mode))

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
	 (go-mode . lsp-deferred)
	 (rustic-mode . lsp-deferred)
	 ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-rust-analyzer-server-display-inlay-hints t
	lsp-rust-analyzer-display-chaining-hints t
	lsp-rust-analyzer-display-parameter-hints t))

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config (setq lsp-ui-flycheck-live-reporting t)
  :hook (lsp-node . lsp-ui-mode))

(use-package dap-mode
  :config
  (dap-mode 1)
  :bind (("C-c a n"     . dap-next)
	 ("C-c a c"     . dap-continue)
	 ("C-c a b t"   . dap-breakpoint-toggle)
	 ("C-c a b d d" . dap-breakpoint-delete)
	 ("C-c a b d a" . dap-breakpoint-delete-all)
	 ("C-c a b a"   . dap-breakpoint-add)
	 ("C-c a b c"   . dap-breakpoint-condition)
	 ("C-c a b h"   . dap-breakpoint-hit-condition)
	 ("C-c a b l"   . dap-breakpoint-log-message)
	 ("C-c a b u"   . dap-ui-breakpoints-list)
	 ("C-c a e"     . dap-eval)
	 ("C-c a r"     . dap-eval-region)
	 ("C-c a s i"   . dap-step-in)
	 ("C-c a s o"   . dap-step-out)
	 ("C-c a d i"   . dap-disconnect)
	 ("C-c a d d"   . dap-debug)
	 ("C-c a d l"   . dap-debug-last)
	 ("C-c a d r"   . dap-debug-recent)
	 ("C-c a o"     . dap-go-to-output-buffer)))



(progn
  (let ((map (if (boundp 'input-decode-map)
		 input-decode-map
	       function-key-map)))
    (define-key map "\e[1;P9"  (kbd "C-;"))
    (define-key map "\e[1;P10"  (kbd "C-."))
    (define-key map "\e[1;P11"  (kbd "<C-return>"))
    (define-key map "\e[1;P12"  (kbd "C-,"))))

;; lsp modes

(use-package origami)

(use-package lsp-origami
  :requires (origami lsp-mode)
  :hook (lsp-after-open-hook . lsp-origami-try-enable))

(setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java"))
;;(require 'lsp-java)
;;(add-hook 'java-mode-hook #'lsp)
(when
    (getenv "JAVA_HOME")
  (use-package lsp-java
    :hook (java-mode . lsp-deferred)))


(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package lsp-docker)
(use-package lsp-focus)
(add-hook 'focus-mode-hook #'lsp-focus-mode)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  ;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-849355222
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
  (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode))
  :config (setq typescript-indent-level 2))

(use-package project
  :straight (project :type built-in))

(when
    (executable-find "cargo")
  (use-package rustic))

;; yasnippet
(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1)
  ;; Bind `C-.' to `yas-expand' when snippet expansion available (it
  ;; will still call `self-insert-command' otherwise).
  (define-key yas-minor-mode-map (kbd "C-.") yas-maybe-expand)

  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

  :diminish
  :bind (:map yas-minor-mode-map
	      ("TAB" . nil)
	      ("<tab>" . nil)))

(use-package yasnippet-snippets
  :after (yasnippet))
(use-package yasnippet-classic-snippets
  :after (yasnippet))

;; disable prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(put 'narrow-to-region 'disabled nil)
;; Show tab bar only when there are multiple tabs
(setq tab-bar-show 1)

(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; show trailing whitespaces
(defun my/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my/show-trailing-whitespace)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(winner-mode)

;; from http://whattheemacsd.com

;; Write backup files to own directory
(let ((backup-dir (file-name-as-directory (expand-file-name
					   (concat user-emacs-directory "backups")))))
  (setq
   backup-directory-alist `(("." . ,backup-dir))
   auto-save-file-name-transforms `((".*" ,backup-dir t))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(save-place-mode 1)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; turn off menu bar
(menu-bar-mode -1)

(defun my/get-line-number-type ()
  "Return visual or relative depending on what mode is enabled."
  (cond ((or
	  selective-display
	  (and (boundp 'hs-minor-mode) hs-minor-mode)
	  (and (boundp 'origami-mode) origami-mode))
	 'visual)
        ((derived-mode-p 'text-mode) 'visual)
        ((derived-mode-p 'prog-mode) 'relative)))

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-hook 'text-mode-hook (lambda ()
			    (setq display-line-numbers 'visual)
			    (visual-line-mode)))

(defun my/enable-folding-mode (hook-enabled)
  (if hook-enabled
      (progn
	(setq display-line-numbers 'visual)
	(visual-line-mode)
	(setq truncate-lines t))
    (progn
      (setq display-line-numbers 'relative)
      (visual-line-mode 0)
      (setq truncate-lines nil))))

(add-hook 'hs-minor-mode-hook (lambda () (my/enable-folding-mode hs-minor-mode)))
(add-hook 'origami-mode-hook (lambda () (my/enable-folding-mode origami-mode)))

(defun my/toggle-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (cond ((string= display-line-numbers (symbol-name (my/get-line-number-type))) (setq display-line-numbers t))
	((string= display-line-numbers "t") (setq display-line-numbers (my/get-line-number-type)))))

(defun my/turn-on-absolute-numbers-for-window (win)
  (when (window-valid-p win)
    (with-selected-window win
      (if display-line-numbers
	  (setq display-line-numbers t)))))

(defun my/switch-relative-numbers-off-previous-window (arg)
  "Switch relative numbers on for current window and off for old window."
  (if (or
       ;; old window was closed
       (not (window-buffer (old-selected-window)))
       ;; going to a window with the same file
       (not (string= (buffer-file-name) (buffer-file-name (window-buffer (old-selected-window))))))
      (progn
	(when display-line-numbers
	  (setq display-line-numbers (my/get-line-number-type)))

	(unless (minibufferp)
	  (my/turn-on-absolute-numbers-for-window (old-selected-window))))))

(defun my/switch-to-normal-mode (win)
  "Switch to normal mode when entering WIN that is in insert mode."
  (interactive)
  (when (and
	 (not (minibufferp))
	 (not (minibufferp (window-buffer (old-selected-window))))
	 (evil-insert-state-p))
    (evil-normal-state)))

(setq window-selection-change-functions '(my/switch-relative-numbers-off-previous-window my/switch-to-normal-mode))

(defun my/turn-relative-numbers-off-other-windows ()
  (interactive)
  (let ((current-window (selected-window)))
    (walk-windows
     (lambda (win)
       (unless (eq win current-window)
	 (my/turn-on-absolute-numbers-for-window win)))
     nil 'visible)))

(global-set-key (kbd "C-c w") 'ace-window)


(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)

(defun set-cursor-bar ()
  (send-string-to-terminal "\e[5 q"))

(defun restore-cursor ()
  (send-string-to-terminal "\e[2 q"))

(global-set-key (kbd "C-c M-;") 'comment-indent-new-line)

(defun my/set-auto-fill-mode ()
  (auto-fill-mode 1)
  (set (make-local-variable 'comment-auto-fill-only-comments) t))
(add-hook 'prog-mode-hook 'my/set-auto-fill-mode)

(defun my/new-comment-line-newline (&rest r)
  "Call `comment-indent-new-line' if inside comment, otherwise `newline' with R."
  (interactive)
  (if (nth 4 (syntax-ppss))
      (comment-indent-new-line)
    (if (called-interactively-p 'any) (call-interactively 'newline) (newline r))))

(use-package evil
  :after evil-leader
  :init
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  ;; do not use evil in magit
  (add-to-list 'evil-buffer-regexps '("\\*magit:"))
  (add-to-list 'evil-buffer-regexps '("\\*org-goto\\*"))
  (add-hook 'evil-insert-state-entry-hook #'set-cursor-bar)
  (add-hook 'evil-insert-state-exit-hook #'restore-cursor)
  (define-key evil-insert-state-map (kbd "RET") #'my/new-comment-line-newline))

(defun my/evil-insert-mode (&rest _)
  "Used in advice for switching to insert mode."
  (evil-insert 1))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-leader
  :config
  (global-evil-leader-mode))

(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") #'xref-goto-xref)
(evil-define-key 'normal occur-mode-map (kbd "RET") #'occur-mode-goto-occurrence)
(evil-define-minor-mode-key 'normal 'magit-blame-mode (kbd "RET") 'magit-show-commit)
(define-key evil-normal-state-map (kbd "z j") 'origami-next-fold)
(define-key evil-normal-state-map (kbd "z k") 'origami-previous-fold)
(define-key evil-normal-state-map (kbd "z e") 'origami-show-only-node)
(define-key evil-normal-state-map (kbd "C-w t") 'tab-new)

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

(evil-global-set-key 'normal (kbd "C-c h") 'evil-ex-nohighlight)

(defun my/evil-search-all-windows (func &rest r)
  "Show highlighting in all visible windows, calling search function FUNC with args R."
  (let ((current-window (selected-window)))
    (apply func r)
    (dolist (win (window-list))
      (unless (eq win current-window)
	(with-selected-window win
	  (evil-ex-search-activate-highlight evil-ex-search-pattern))))))

(advice-add 'evil-ex-start-search      :around #'my/evil-search-all-windows)
(advice-add 'evil-ex-start-word-search :around #'my/evil-search-all-windows)

(defun my/evil-no-highlight (func &rest r)
  "Removes highlight in all windows by calling FUNC with args R."
  (dolist (win (window-list))
    (with-selected-window win
      (apply func r))))

(advice-add 'evil-ex-nohighlight :around #'my/evil-no-highlight)

(defun my/next-error ()
  (interactive)
  (cond ((derived-mode-p 'compilation-mode) (next-error))
	(t (flycheck-next-error))))

(defun my/prev-error ()
  (interactive)
  (cond ((derived-mode-p 'compilation-mode) (previous-error))
	(t (flycheck-previous-error))))

(dolist (map (list evil-normal-state-map evil-motion-state-map))
  (define-key map (kbd "] g") 'my/next-error)
  (define-key map (kbd "[ g") 'my/prev-error))
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(evil-define-minor-mode-key 'normal 'winner-mode-map (kbd "] u") 'winner-redo)
(evil-define-minor-mode-key 'normal 'winner-mode-map (kbd "[ u") 'winner-undo)

;; https://emacs.stackexchange.com/a/16825
(defun my/only-whitespace-p ()
  "Return t if line is nonempty and only whitespace."
  (and
   (> (current-indentation) 0)
   (= (current-indentation)
      (- (line-end-position) (line-beginning-position)))))

(defun my/kill-line-if-only-whitespace (&rest _)
  "Kill the current line if it is only made up of whitespace."
  (when (my/only-whitespace-p)
    (beginning-of-line)
    (kill-line)))

(defun my/open-line-above ()
  (interactive)
  (beginning-of-line)
  (my/kill-line-if-only-whitespace)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(advice-add 'evil-normal-state :after #'my/kill-line-if-only-whitespace)

(evil-global-set-key 'insert (kbd "<C-return>") 'my/open-line-above)

;; prevent electric pair when searching
(defun my/temp-disable-electric-pair (func &rest r)
  "Disable and enable electric pair mode around FUNC with params R."
  (electric-pair-mode -1)
  (unwind-protect (apply func r) (electric-pair-mode +1)))

(advice-add 'evil-ex-search-forward :around #'my/temp-disable-electric-pair)
(advice-add 'evil-ex-search-backward :around #'my/temp-disable-electric-pair)
;; Disable pairs when entering minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode -1)))
;; Renable pairs when existing minibuffer
(add-hook 'minibuffer-exit-hook (lambda () (electric-pair-mode 1)))

(use-package magit
  :bind (("C-c g" . 'magit-file-dispatch)
	 ("C-c G" . 'magit-dispatch))
  :config
  (setq magit-keep-region-overlay t)
  (setq magit-diff-refine-hunk 'all))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(dolist (m (list magit-status-mode-map magit-diff-mode-map))
  (define-key m (kbd "C-u C-j") 'magit-diff-visit-worktree-file-other-window))

;; https://emacs.stackexchange.com/a/13831
(defun magit-diff-mbase-master (&optional args)
  "Show diff of $(git diff merge-base master HEAD) to working tree with optional ARGS."
  (interactive (list (magit-diff-arguments)))
  (magit-diff-working-tree
   (magit-git-string "merge-base" "origin/master" "HEAD") (car args) (cadr args)))

(transient-append-suffix 'magit-diff "w" '("m" "Diff merge-base master" magit-diff-mbase-master))

(defun magit-diff-mbase-other (&optional args)
  "Show diff of $(git diff merge-base <branch> HEAD) to working tree with optional ARGS."
  (interactive (list (magit-diff-arguments)))
  (magit-diff-working-tree
   (magit-git-string "merge-base"
		     (magit-read-other-branch "Select start of merge-base")
		     "HEAD")
   (car args) (cadr args)))

(transient-append-suffix 'magit-diff "m" '("o" "Diff merge-base other" magit-diff-mbase-other))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(unless (= (display-color-cells) 16777216) ; true color support
  (set-face-attribute 'ediff-current-diff-A nil :extend t :background "red" :foreground "brightwhite")
  (set-face-attribute 'ediff-current-diff-Ancestor nil :extend t :background "#ccc6d1" :foreground "black")
  (set-face-attribute 'ediff-current-diff-B nil :extend t :background "green" :foreground "brightwhite")
  (set-face-attribute 'ediff-current-diff-C nil :extend t :background "blue" :foreground "brightwhite")
  (set-face-attribute 'ediff-fine-diff-A nil :foreground "color-255")
  (set-face-attribute 'ediff-fine-diff-Ancestor nil :background "#b6b0d6" :foreground "black")
  (set-face-attribute 'ediff-fine-diff-B nil :foreground "color-255")
  (set-face-attribute 'ediff-fine-diff-C nil :background "color-45" :foreground "color-240"))


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

(use-package apheleia
  :disabled t
  :straight (:host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode +1))

(use-package format-all
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter)))

(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
				  :compile "npm install"
				  :test "npm test"
				  :run "npm start"
				  :test-suffix ".spec")

(use-package jest
  :straight (:host github :repo "Emiller88/emacs-jest")
  :hook ((js2-mode        . jest-minor-mode)
	 (js-mode         . jest-minor-mode)
	 (typescript-mode . jest-minor-mode)))

(use-package lsp-ivy)

(global-set-key (kbd "C-x t t") 'treemacs)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :straight (lsp-treemacs :host github :repo "emacs-lsp/lsp-treemacs"))


(use-package org
  :straight (org :type built-in)
  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "<C-return>") #'org-insert-heading-respect-content)

  (add-hook 'org-mode-hook 'flyspell-mode)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAIT(w@/!)" "|" "DONE(d@)" "CANCELED(c@)")))

  (setq org-goto-auto-isearch nil)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(use-package evil-org
  :after (evil org)
  :straight (:host github :repo "Somelauw/evil-org-mode")
  :hook ((org-mode . evil-org-mode)))

(when
    (executable-find "fzf")
  (use-package fzf
    :straight (:host github :repo "bling/fzf.el")))

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
	   (("call" .  "print(f\"\")")
	    ("placeholder" . "{=}")
	    ("char-to-insert-at" . "=")
	    ("skip-log-word" . t)))))
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
	    (insert
	     (file-relative-name buffer-file-name (projectile-project-root))
	     ":"
	     (int-to-string (line-number-at-pos))
	     " - "
	     (if
		 (not (my/alist-get-symbol "skip-log-word" log-info-from-alist))
		 (concat current-word ": ")
	       "")
	     (my/alist-get-symbol "placeholder" log-info-from-alist ""))
	    (move-end-of-line nil)
	    (if-let ((search-char (my/alist-get-symbol "char-to-insert-at" log-info-from-alist)))
		(search-backward search-char nil t)
	      (backward-char))
	    (when-let ((seperator (my/alist-get-symbol "seperator" log-info-from-alist)))
	      (insert seperator " "))
	    (if-let ((fun-to-call (my/alist-get-symbol "function-to-call" log-info-from-alist)))
		(funcall (intern fun-to-call) current-word)
	      (insert current-word))
	    (move-end-of-line nil)
	    (insert (my/alist-get-symbol "eol-char" log-info-from-alist ""))))))))

(defun setup-logging (map-to-add)
  (define-key map-to-add (kbd "C-c o") 'log-word-at-point))

(add-hook 'python-mode-hook (lambda () (setup-logging python-mode-map)))
(add-hook 'js-mode-hook (lambda () (setup-logging js-mode-map)))
(add-hook 'js2-mode-hook (lambda () (setup-logging js2-mode-map)))
(add-hook 'typescript-mode-hook (lambda () (setup-logging typescript-mode-map)))
(add-hook 'go-mode-hook (lambda () (setup-logging go-mode-map)))

(defun my/json-stringify (&optional word)
  (interactive)
  (save-excursion
    (save-match-data
      (let ((thing (if word word (thing-at-point 'symbol 'no-properties)))
	    (bounds (bounds-of-thing-at-point 'symbol)))
	(progn
	  (when (and bounds (not word))
	    (kill-region (car bounds) (cdr bounds)))
	  (insert "JSON.stringify(" thing ", null, 2)"))))))

(defun my/determine-case (word)
  "Determine case of WORD."
  (let ((case-fold-search nil))
    (cond
     ((string-match-p "^[A-Z]+\\(_[A-Z]+\\)*$" word) "UPPER_CASE_SNAKE_CASE")
     ((string-match-p "_"                      word) "snake_case")
     ((string-match-p "-"                      word) "kebab-case")
     ((string-match-p "^[A-Z]"                 word) "PascalCase")
     (t                                              "camelCase"))))

(defun my/split-word (word)
  "Split WORD into words. Use optional CASE as the case of the word."
  (let ((case-fold-search nil)
	(case (my/determine-case word)))
    (cond
     ((string= case "UPPER_CASE_SNAKE_CASE") (split-string word "_"))
     ((string= case "snake_case")            (split-string word "_"))
     ((string= case "kebab-case")            (split-string word "-"))
     ((string= case "PascalCase")            (s-slice-at "[A-Z]" word))
     ((string= case "camelCase" )            (s-slice-at "[A-Z]" word)))))

(defun my/convert-words-to-case (words case)
  "Combine words in WORDS into one word with case CASE."
  (cond
   ((string= case "UPPER_CASE_SNAKE_CASE") (mapconcat 'upcase words "_"))
   ((string= case "snake_case")            (mapconcat 'downcase words "_"))
   ((string= case "kebab-case")            (mapconcat 'downcase words "-"))
   ((string= case "PascalCase")            (mapconcat 'capitalize words ""))
   ((string= case "camelCase" )            (concat
					    (downcase (car words))
					    (mapconcat 'capitalize (cdr words) "")))))

(defun my/change-case ()
  "Change case of the current word or region."
  (interactive)
  (save-excursion
    (save-match-data
      (let* ((current-word
	      (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(thing-at-point 'symbol 'no-properties)))
	     (bounds (if (region-active-p)
			 (cons (region-beginning) (region-end))
		       (bounds-of-thing-at-point 'symbol)))
	     (target-case (completing-read
			   "Choose a case: "
			   '("camelCase" "snake_case" "kebab-case" "PascalCase" "UPPER_CASE_SNAKE_CASE")
			   nil t)))
	(kill-region (car bounds) (cdr bounds))
	(insert (my/convert-words-to-case (my/split-word current-word) target-case))))))

(defvar dir-name nil "Directory name for the current session.")

(defun my/desktop-save (session-name)
  (interactive "sSession name: ")
  (setq dir-name (concat "~/.emacs.d/desktops/" session-name "/"))
  (unless (file-directory-p dir-name) (make-directory dir-name))
  (setq desktop-path (list dir-name))
  (desktop-save dir-name)
  (desktop-save-mode 1)
  (my/update-session-mode-line))

(defun my/desktop-read ()
  (interactive)
  (my/save-and-release-desktop t)
  (setq dir-name (read-directory-name "Directory: " "~/.emacs.d/desktops/"))
  (setq desktop-path (list dir-name))
  (advice-remove 'dired #'split-in-direction)
  (desktop-read dir-name)
  (desktop-save-mode 1)
  (my/turn-relative-numbers-off-other-windows)
  (advice-add 'dired :before #'split-in-direction)
  (my/update-session-mode-line))

(defun my/save-and-release-desktop (&optional should-release)
  (interactive)
  "Save and release desktop which removes lock file."
  (let ((should-release (or should-release (yes-or-no-p "Release desktop when saving?"))))
    (if (and desktop-save-mode desktop-path dir-name)
	;; save existing desktop
	(progn
	  (desktop-save dir-name should-release)
	  (when should-release (desktop-save-mode 0))
	  (message "Saved desktop.")
	  (setq dir-name nil)
	  (my/update-session-mode-line))
      (when (called-interactively-p 'any)
	(user-error "No current desktop to save")))))

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

(defun my/update-session-mode-line ()
  "Update session-mode-line-string."
  (setq session-mode-line-string (concat " " (if
						 (and (boundp 'dir-name) dir-name)
						 (file-name-nondirectory
						  (directory-file-name
						   (file-name-directory dir-name)))
					       "No session")
					 " ")))

(my/update-session-mode-line)

(add-to-list 'global-mode-string '(:eval session-mode-line-string))

(defun my/shorten-path (path)
  (let* ((file-only (file-name-nondirectory path))
	 (dir (file-name-directory path))
	 (short-path (string-join (mapcar (lambda (part)
					    (let ((first-letter (substring part 0 1)))
					      (if (string= first-letter ".")
						  (substring part 0 2) first-letter))) (split-string dir "/" t)) "/")))
    (format "%s/%s" short-path file-only)))

;; https://www.reddit.com/r/emacs/comments/8xobt3/tip_in_modeline_show_buffer_file_path_relative_to
(with-eval-after-load 'subr-x
  (setq-default mode-line-buffer-identification
                '(:eval (format-mode-line (propertized-buffer-identification (or (when-let*
										     ((buffer-file-truename buffer-file-truename)
										      (prj (cdr-safe (project-current)))
										      (prj-parent (file-name-directory (directory-file-name (expand-file-name prj))))
										      (my-mode-line-name (file-relative-name buffer-file-truename prj))
										      (ratio (/ (window-total-width) (length my-mode-line-name))))
										   (cond ((and (<= ratio 2) (> ratio 1))
											  (my/shorten-path my-mode-line-name))
											 ((<= ratio 1) (buffer-name))
											 (t my-mode-line-name)))
										 "%b"))))))

(use-package rainbow-delimiters
  :straight (:host github :repo "Fanael/rainbow-delimiters")
  :hook ((prog-mode . rainbow-delimiters-mode)))

(defun my/set-cliboard-around-command (func &rest r)
  "Execute command with and without clipboard."
  (setq select-enable-clipboard t)
  (apply func r)
  (setq select-enable-clipboard nil))

(advice-add 'dired-copy-filename-as-kill :around #'my/set-clipboard-around-command)

(defun my/org-copy-link ()
  "Copy org link under point."
  (interactive)
  (let ((current-context (org-element-context)))
    (if (not (org-element-type current-context))
	(user-error "No link found at point")
      (kill-new (org-element-property :raw-link current-context)))))

(advice-add 'my/org-copy-link :around #'my/set-cliboard-around-command)

(defun my/fix-insert (func &rest r)
  "Apply FUNC with args R and then restore cursor after."
  (interactive)
  (if (called-interactively-p 'any) (call-interactively func) (apply func r))
  (restore-cursor))

(advice-add 'async-shell-command :around #'my/fix-insert)
(advice-add 'dap-debug           :around #'my/fix-insert)

;; https://www.reddit.com/r/emacs/comments/dfxe1u/codefolding_based_off_indent_level/f370ish/?utm_source=reddit&utm_medium=web2x&context=3
(defun my/toggle-indentation-fold ()
  "Toggle code folding according to indentation of current line."
  (interactive)
  (set-selective-display
   (if selective-display
       (progn
	 (setq truncate-lines nil)
	 (visual-line-mode 0)
	 (setq display-line-numbers 'relative)
	 nil)
     (progn
       (setq truncate-lines t)
       (visual-line-mode)
       (setq display-line-numbers 'visual)
       (save-excursion
	 (back-to-indentation)
	 (1+ (current-column)))))))
(global-set-key (kbd "C-c C-f") #'my/toggle-indentation-fold)

;; https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
		   scroll-down-command
		   recenter-top-bottom
		   other-window
		   evil-window-next
		   evil-scroll-line-to-center
		   evil-scroll-line-to-top
		   evil-scroll-line-to-bottom
		   evil-window-up
		   evil-window-down
		   evil-window-right
		   evil-window-left
		   evil-window-top
		   evil-window-bottom
		   evil-window-bottom-right
		   evil-window-top-left
		   evil-window-mru
		   evil-window-delete
		   xref-goto-xref
		   lsp-find-definition))
  (advice-add command :after #'pulse-line))

(defun my/escape (characters beg end)
  "Escape characters in CHARACTERS between BEG and END."
  (interactive "sCharacters to escape: \nr")
  (unless (region-active-p) (user-error "Please select a region before calling this function"))
  (when (string-empty-p characters) (user-error "Please input charactes"))
  (save-excursion
    (let* ((character-list (s-slice-at "." characters))
	   (current-char (car character-list)))
      (unless character-list
	(user-error "No characters found"))
      (while character-list
	(goto-char beg)
	(while (re-search-forward (concat "\\([^\\]\\)" current-char) end t)
	  (replace-match (concat (match-string 1) "\\\\" current-char))
	  (setq end (1+ end)))
	(setq character-list (cdr character-list))
	(setq current-char (car character-list)))))
  (if evil-mode (evil-exit-visual-state) (deactivate-mark)))

;; ansi color support in compilation-mode
;; https://stackoverflow.com/a/20788581/5521899
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(advice-add #'previous-line-or-history-element :after #'move-end-of-line)

(defun my/with-projectile-root(func &rest r)
  "Run FUNC with args R with default directory set to `(projectile-project-root)'."
  (let ((default-directory (projectile-project-root)))
    (apply func r)))
(advice-add 'find-file-at-point                :around #'my/with-projectile-root)
(advice-add 'evil-find-file-at-point-with-line :around #'my/with-projectile-root)
(advice-add 'compile                           :around #'my/with-projectile-root)

(use-package tree-sitter-langs)
(use-package tree-sitter
  ;; :init
  ;; (setq tree-sitter-hl-use-font-lock-keywords nil)
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (set-face-attribute 'tree-sitter-hl-face:function.call nil
		      :inherit '(link font-lock-function-name-face)
		      :foreground "#458488"
		      :underline nil)
  ;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-849355222
  (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/discussions/132#discussioncomment-502873
;;;; Smart f-strings
;; https://github.com/ubolonton/emacs-tree-sitter/issues/52
(defun my/python-f-string-ify (&rest _)
  ;; Does nothing if major-mode is not python or point is not on a string.
  (when-let* ((python-mode-p (eq major-mode 'python-mode))
              (str (tree-sitter-node-at-point 'string))
              (text (ts-node-text str)))
    (let ((is-f-string (string-match-p "^[bru]*f+[bru]*\\(\"\\|'\\)" text))
          (should-f-string (and (s-contains-p "{" text)
                                (s-contains-p "}" text))))
      (if should-f-string
          (unless is-f-string
            (save-excursion
              (goto-char (ts-node-start-position str))
              (insert "f")))
        (when is-f-string
          (save-excursion
            (goto-char (ts-node-start-position str))
            (when (char-equal (char-after) ?f)
              (delete-char 1))))))))

(define-key python-mode-map (kbd "{") (lambda ()
                                        (interactive)
                                        (call-interactively 'self-insert-command)
                                        (my/python-f-string-ify)))

(advice-add 'wrap-region-trigger  :after #'my/python-f-string-ify)
(advice-add 'delete-char          :after #'my/python-f-string-ify)
(advice-add 'delete-active-region :after #'my/python-f-string-ify)
(advice-add 'evil-delete          :after #'my/python-f-string-ify)
(advice-add 'evil-surround-region :after #'my/python-f-string-ify)
(advice-add 'evil-surround-change :after #'my/python-f-string-ify)
(advice-add 'evil-surround-delete :after #'my/python-f-string-ify)

(defun my/replace-char (chars replace)
  "Replace char at point with REPLACE if any of CHARS."
  (when (seq-some
	 (lambda (char)
	   (char-equal (char-after) char))
	 chars)
    (delete-char 1)
    (insert replace)))

(defun my/template-literal-backtick-ify (&rest _)
  (when-let* ((ts-or-js-mode-p (member major-mode '(typescript-mode js-mode)))
              (str (or (tree-sitter-node-at-point 'string) (tree-sitter-node-at-point 'template_string)))
              (text (ts-node-text str)))
    (let ((is-template-literal (tree-sitter-node-at-point 'template_string))
	  (should-template-literal (and (s-contains-p "${" text)
					(s-contains-p "}" text))))
      (if should-template-literal
          (unless is-template-literal
            (save-excursion
	      (dolist (pos '((ts-node-start-position str) (ts-node-end-position str)))
		(goto-char (eval pos))
		(my/replace-char (list ?\" ?') "`"))))
        (when is-template-literal
          (save-excursion
	    (dolist (pos '((ts-node-start-position str) (ts-node-end-position str)))
	      (goto-char (eval pos))
	      (my/replace-char (list ?`) "\""))))))))

(with-eval-after-load 'typescript-mode
  (when (and (boundp 'typescript-mode-map) typescript-mode-map)
    (define-key typescript-mode-map (kbd "{") (lambda ()
						(interactive)
						(call-interactively 'self-insert-command)
						(my/template-literal-backtick-ify)))))

(advice-add 'wrap-region-trigger  :after #'my/template-literal-backtick-ify)
(advice-add 'delete-char          :after #'my/template-literal-backtick-ify)
(advice-add 'delete-active-region :after #'my/template-literal-backtick-ify)
(advice-add 'evil-delete          :after #'my/template-literal-backtick-ify)
(advice-add 'evil-surround-region :after #'my/template-literal-backtick-ify)
(advice-add 'evil-surround-change :after #'my/template-literal-backtick-ify)
(advice-add 'evil-surround-delete :after #'my/template-literal-backtick-ify)

(defun my/sort-js-object (&optional pos)
  "Sort JS/TS object alphabetically by keys, optionally with the object at POS."
  (interactive)
  (let* ((starting-pos (or pos (point)))
	 (cursor (tsc-make-cursor (tree-sitter-node-at-pos 'object starting-pos)))
	 (node (tsc-current-node cursor))
	 (pair-nodes (list))
	 sorted-pairs pair-seperator start-seperator end-seperator)
    ;; Get all pairs.
    (dotimes (child-num (tsc-count-children node))
      (let ((child (tsc-get-nth-child node child-num)))
	(when (equal (tsc-node-type child) 'pair)

	  ;; Save the text from the start to the beginning of the first seperator.
	  (when (= (length pair-nodes) 0)
	    (setq start-seperator (buffer-substring-no-properties
				   (tsc-node-start-position node)
				   (tsc-node-start-position child))))

	  ;; Save separator in between the pairs.
	  (when (= (length pair-nodes) 1)
	    (setq pair-seperator (buffer-substring-no-properties
				  (tsc-node-end-position (car pair-nodes))
				  (tsc-node-start-position child))))

	  ;; if we find a nested object, we have to recursively sort any objects inside
	  (when-let* ((potential-object-node (tsc-get-nth-child child 2))
		      (is-object-p (equal (tsc-node-type potential-object-node) 'object)))
	    (my/sort-js-object (tsc-node-start-position potential-object-node))

	    ;; The nodes have been updates, so we have to parse the object again
	    ;; and gather the pairs.
	    (setq cursor (tsc-make-cursor (tree-sitter-node-at-pos 'object starting-pos)))
	    (setq node (tsc-current-node cursor))
	    (setq pair-nodes (list))

	    ;; Grab all pairs and current child again.
	    ;; This is simpler since all prior things have been sorted and seperators
	    ;; have been obtained.
	    (dotimes (redo-child-num child-num)
	      (let ((redo-child (tsc-get-nth-child node redo-child-num)))
		(when (equal (tsc-node-type redo-child) 'pair)
		  (setq pair-nodes (append pair-nodes (list redo-child))))))
	    (setq child (tsc-get-nth-child node child-num)))

	  (setq pair-nodes (append pair-nodes (list child))))))

    ;; Save the text from the end of the last pair to the end.
    (setq end-seperator (buffer-substring-no-properties
			 (tsc-node-end-position (car (last pair-nodes)))
			 (tsc-node-end-position node)))

    (setq sorted-pairs (string-join
			;; Grab the text of each node.
			(mapcar 'tsc-node-text
				;; Sort on the keys.
				(sort pair-nodes (lambda (first second)
						   (string<
						    ;; The key is the first child of a pair node.
						    (tsc-node-text (tsc-get-nth-child first 0))
						    (tsc-node-text (tsc-get-nth-child second 0))))))
			pair-seperator))
    ;; Update the text in the buffer;
    (goto-char (tsc-node-start-position node))
    (kill-region (tsc-node-start-position node) (tsc-node-end-position node))
    (insert (concat start-seperator sorted-pairs end-seperator))))

(defun my/goto-top-object ()
  "Move point to the beginning of the topmost object."
  (interactive)
  (let* ((cursor (tsc-make-cursor (tree-sitter-node-at-point 'object)))
	 (node (tsc-current-node cursor)))
    (while (or
	    (equal (tsc-node-type (tsc-get-parent node)) 'object)
	    (equal (tsc-node-type (tsc-get-parent node)) 'pair))
      (setq node (tsc-get-parent node)))
    (goto-char (tsc-node-start-position node))))

(defun my/goto-top-object-and-sort ()
  "Move point to the topmost object and sort."
  (interactive)
  (my/goto-top-object)
  (my/sort-js-object))

(defun my/change-window-layout (func)
  "Change window layout based on FUNC."
  (let ((win-length (length (window-list)))
	(num-windows-processed 0)
	(win (car-safe (window-list))))
    (while (and win (< num-windows-processed win-length))
      (with-selected-window win
	(funcall func)
	(other-window 1)
	(setq win (car-safe (window-list)))
	(setq num-windows-processed (1+ num-windows-processed))))))

(defun my/split-windows-horizontal ()
  "Splits all windows horizontally."
  (interactive)
  (my/change-window-layout 'evil-window-move-very-bottom))

(defun my/split-windows-vertical ()
  "Splits all windows vertically."
  (interactive)
  (my/change-window-layout 'evil-window-move-far-right))

(defun my/goto-prev-indentation-level ()
  "Move point to previous indentation level in file."
  (interactive)
  (back-to-indentation)
  (let ((current-indentation-level (current-indentation)))
    (while (and (not (bolp)) (<= current-indentation-level (current-indentation)))
      (if (fboundp 'evil-previous-line)
	  (evil-previous-line)
	(forward-line -1))))
  (when (fboundp 'evil-set-jump)
    (evil-set-jump)
    (back-to-indentation)))

(global-set-key (kbd "C-c u") #'my/goto-prev-indentation-level)

(use-package csv-mode)

(defun my/get-csv-field-name ()
  "Print column that cursor is in."
  (interactive)
  (unless (eq major-mode 'csv-mode)
    (user-error "Please use this in csv-mode"))
  (when-let* ((field-name csv-field-index-string)
	      (dest-index (string-to-number (substring field-name 1))))
    (save-excursion
      (beginning-of-buffer)
      (print (nth dest-index
		  (split-string
		   (buffer-substring-no-properties
		    (line-beginning-position)
		    (line-end-position))
		   ","))))))

(global-set-key (kbd "C-c q") #'quit-window)

(defun my/recreate-buffer ()
  "Kill and replace the current buffer."
  (interactive)
  (let ((buffer-name buffer-file-truename))
    (kill-this-buffer)
    (find-file buffer-name)))

(defun my/number-of-windows-wide(&optional num-of-windows window)
  (interactive)
  (let* ((current-window (or window (selected-window)))
	 (num-of-windows (or num-of-windows 0))
	 (window-at-right-side (window-at-side-p current-window 'right))
	 )
    (if window-at-right-side (1+ num-of-windows)
      (max
       (my/number-of-windows-wide (1+ num-of-windows)
				  (window-in-direction 'right current-window nil +1))
       (my/number-of-windows-wide (1+ num-of-windows)
				  (window-in-direction 'right current-window nil -1))
       (if (and
	    (window-at-side-p current-window 'left)
	    (not (window-at-side-p current-window 'bottom))
	    (window-at-side-p (window-in-direction 'down current-window nil +1) 'left))
	   (my/number-of-windows-wide 0 (window-in-direction 'down current-window nil +1))
	 0)))))

(defun my/balance-window-widths ()
  "Balance all window widths."
  (interactive)
  (let* ((win (frame-first-window))
	 (num-windows-wide (my/number-of-windows-wide 0 win))
	 (desired-width (/ (frame-text-width) num-windows-wide)))
    (balance-windows)
    (dotimes (_ (count-windows))
      (with-selected-window win
	(enlarge-window-horizontally (- desired-width (window-width))))
      (setq win (next-window)))
    (dotimes (_ (count-windows))
      (with-selected-window win
	(enlarge-window-horizontally (- desired-width (window-width)))
	(setq win (previous-window))))))

(define-key evil-motion-state-map [remap balance-windows] 'my/balance-window-widths)

(defun my/evil-custom-paste (before-or-after type &optional register)
  (let* ((register (or register ?\"))
	 (text (evil-get-register register))
	 (command (cond ((eq before-or-after 'before) 'evil-paste-before)
			((eq before-or-after 'after) 'evil-paste-after)
			(t nil))))
    (unless command (user-error "Please supply a valid command"))

    (set-text-properties 0 (length text) nil text)
    (unless type (setq text (string-trim text)))

    ;; `filter-buffer-substring' is used to get the text that will be yanked.
    ;; We want to override that so it returns our specified text instead.
    (advice-add 'filter-buffer-substring :override (lambda (&rest _) text) '((name . "test")))

    ;; Perform the yank with the specified type
    (evil-yank 0 0 type register)
    (advice-remove 'filter-buffer-substring "test")

    ;; Call the paste function to insert our text
    (funcall command '(1 register))))

(evil-define-command my/evil-paste-after-linewise (&optional register)
  (interactive "<x>")
  (my/evil-custom-paste 'after 'line register))

(evil-define-command my/evil-paste-after-characterwise (&optional register)
  (interactive "<x>")
  (my/evil-custom-paste 'after nil register))

(evil-define-command my/evil-paste-before-linewise (&optional register)
  (interactive "<x>")
  (my/evil-custom-paste 'before 'line register))

(evil-define-command my/evil-paste-before-characterwise (&optional register)
  (interactive "<x>")
  (my/evil-custom-paste 'before nil register))

(evil-leader/set-key (kbd "l p") 'my/evil-paste-after-linewise)
(evil-leader/set-key (kbd "l P") 'my/evil-paste-before-linewise)
(evil-leader/set-key (kbd "c p") 'my/evil-paste-after-characterwise)
(evil-leader/set-key (kbd "c P") 'my/evil-paste-before-characterwise)

(use-package hl-todo
  :after evil
  :config
  (global-hl-todo-mode)
  (define-key evil-normal-state-map (kbd "] t") 'hl-todo-next)
  (define-key evil-normal-state-map (kbd "[ t") 'hl-todo-previous)
  (advice-add 'hl-todo-insert :after 'my/evil-insert-mode))

(defun my/prompt-indent-tabs (func &rest r)
  "Call function FUNC with args R asking user for value for `indent-tabs-mode'."
  (let ((indent-tabs-mode (yes-or-no-p "Use tabs for indentation?")))
    (apply func r)))

(advice-add 'align-regexp :around #'my/prompt-indent-tabs)

(provide '.emacs)

;;; .emacs ends here
