
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
 '(ediff-fine-diff-A ((t (:foreground "color-255"))))
 '(ediff-fine-diff-Ancestor ((t (:background "#b6b0d6" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:foreground "color-255"))))
 '(ediff-fine-diff-C ((t (:background "color-45" :foreground "color-240")))))

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
(setq use-package-always-ensure t)

(use-package diminish)

(autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
(use-package docker-tramp)
(use-package emacsql-psql)
(use-package python-pytest)
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
  (load-theme 'gruvbox-light-medium t))

;; auto switch theme by time of day
;; https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
(defvar current-theme nil)

(defun synchronize-theme ()
  "Set theme based on time of day."
  (let* ((hour (string-to-number (substring (current-time-string) 11 13)))
	 (theme-to-change-to (if (member hour (number-sequence 6 20))
				 'gruvbox-light-medium 'gruvbox)))
    (when (not (equal theme-to-change-to current-theme))
      (setq current-theme theme-to-change-to)
      (load-theme theme-to-change-to t)
      (when (string= system-type "darwin")
	(shell-command "~/Library/ApplicationSupport/iTerm2/iterm2env/versions/3.8.6/bin/python3 ~/Library/ApplicationSupport/iTerm2/Scripts/iterm2-light-dark-toggle.py")))))

(synchronize-theme)
(run-with-timer 0 300 'synchronize-theme)

(use-package free-keys)
(use-package json-mode)

(use-package expand-region
    :bind ("C-^" . .er/expand-region))

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
     ("t" find-file-new-tab "open in a new tab")))

  :bind (("C-c K" . #'counsel-ag)
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

(use-package terraform-mode)

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
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq read-process-output-max (* 1024 1024)))

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
  :bind (("C-c a n"   . dap-next)
	 ("C-c a c"   . dap-continue)
	 ("C-c a b t" . dap-breakpoint-toggle)
	 ("C-c a b d" . dap-breakpoint-delete)
	 ("C-c a b a" . dap-breakpoint-add)
	 ("C-c a b c" . dap-breakpoint-condition)
	 ("C-c a b h" . dap-breakpoint-hit-condition)
	 ("C-c a b l" . dap-breakpoint-log-message)
	 ("C-c a e"   . dap-eval)
	 ("C-c a r"   . dap-eval-region)
	 ("C-c a s i" . dap-step-in)
	 ("C-c a s o" . dap-step-out)
	 ("C-c a d i" . dap-disconnect)
	 ("C-c a d d" . dap-debug)
	 ("C-c a d l" . dap-debug-last)
	 ("C-c a d r" . dap-debug-recent)
	 ("C-c a o"   . dap-go-to-output-buffer)))



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
(use-package lsp-java
  :hook (java-mode . lsp-deferred))


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
  :config (setq typescript-indent-level 2))


;; yasnippet
(use-package yasnippet
  :config (yas-global-mode 1)
  :diminish
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)))

;; Bind `C-.' to `yas-expand' when snippet expansion available (it
;; will still call `self-insert-command' otherwise).
(define-key yas-minor-mode-map (kbd "C-.") yas-maybe-expand)

;;(define-key yas-minor-mode-map (kbd "TAB") yas-next-field-or-maybe-expand)

;; Bind `C-c y' to `yas-expand' ONLY.
(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

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

(winner-mode)

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

(defun my/switch-to-normal-mode (win)
  "Switch to normal mode when entering a window that is in insert mode."
  (interactive)
  (when (evil-insert-state-p) (evil-normal-state)))

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

(global-set-key (kbd "M-j") 'comment-indent-new-line)

(defun my/evil-append (&rest _)
  (end-of-line)
  (evil-append 1))

(use-package evil
  :config
  (evil-mode 1)
  ;; do not use evil in magit
  (add-to-list 'evil-buffer-regexps '("\\*magit:"))
  (add-to-list 'evil-buffer-regexps '("\\*org-goto\\*"))
  (add-hook 'evil-insert-state-entry-hook #'set-cursor-bar)
  (add-hook 'evil-insert-state-exit-hook #'restore-cursor)
  (advice-add 'comment-indent-new-line :after 'my/evil-append))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") #'xref-goto-xref)
(evil-define-key 'normal occur-mode-map (kbd "RET") #'occur-mode-goto-occurrence)
(evil-define-minor-mode-key 'normal 'magit-blame-mode (kbd "RET") 'magit-show-commit)
(define-key evil-normal-state-map (kbd "z j") 'origami-next-fold)
(define-key evil-normal-state-map (kbd "z k") 'origami-previous-fold)
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

(evil-global-set-key 'normal (kbd "M-H") 'evil-ex-nohighlight)

(defun my/next-error ()
  (interactive)
  (cond ((eq major-mode 'compilation-mode)
	 (next-error))
	(t (flycheck-next-error))))

(defun my/prev-error ()
  (interactive)
  (cond ((eq major-mode 'compilation-mode)
	 (previous-error))
	(t (flycheck-previous-error))))

(dolist (map (list evil-normal-state-map evil-motion-state-map))
  (define-key map (kbd "] g") 'my/next-error)
  (define-key map (kbd "[ g") 'my/prev-error))
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(evil-define-minor-mode-key 'normal 'winner-mode-map (kbd "] u") 'winner-redo)
(evil-define-minor-mode-key 'normal 'winner-mode-map (kbd "[ u") 'winner-undo)

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(evil-global-set-key 'insert (kbd "<C-return>") 'open-line-above)

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
  "Show diff of $(git diff merge-base master HEAD) to working tree."
  (interactive (list (magit-diff-arguments)))
  (magit-diff-working-tree
   (magit-git-string "merge-base" "origin/master" "HEAD") args))

(transient-append-suffix 'magit-diff "w" '("m" "Diff merge-base master" magit-diff-mbase-master))

(defun magit-diff-mbase-other (&optional args)
  "Show diff of $(git diff merge-base master HEAD) to working tree."
  (interactive (list (magit-diff-arguments)))
  (magit-diff-working-tree
   (magit-git-string "merge-base"
		     (magit-read-other-branch "Select start of merge-base")
		     "HEAD") args))

(transient-append-suffix 'magit-diff "m" '("o" "Diff merge-base other" magit-diff-mbase-other))

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

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode +1))

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

(use-package evil-org
  :after evil
  :straight (:host github :repo "Somelauw/evil-org-mode")
  :hook ((org-mode . evil-org-mode)))

(add-hook 'org-mode-hook 'evil-org-mode)

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
(evil-define-key 'insert org-mode-map (kbd "<C-return>") #'org-insert-heading-respect-content)

(add-hook 'org-mode-hook 'flyspell-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

(use-package fzf
  :straight (:host github :repo "bling/fzf.el"))

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
  (my/save-and-release-desktop)
  (setq dir-name (read-directory-name "Directory: " "~/.emacs.d/desktops/"))
  (setq desktop-path (list dir-name))
  (advice-remove 'dired #'split-in-direction)
  (desktop-read dir-name)
  (desktop-save-mode 1)
  (my/turn-relative-numbers-off-other-windows)
  (advice-add 'dired :before #'split-in-direction)
  (my/update-session-mode-line))

(defun my/save-and-release-desktop ()
  (interactive)
  "Save and release desktop which removes lock file."
  (if (and desktop-save-mode desktop-path dir-name)
      ;; save existing desktop
      (progn
	(desktop-save dir-name t)
	(desktop-save-mode 0)
	(message "Saved desktop.")
	(setq dir-name nil)
	(my/update-session-mode-line))
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
(defun my/fix-insert-after-command (&rest r)
  (interactive)
  (restore-cursor))

(advice-add 'async-shell-command :after #'my/fix-insert-after-command)

;; https://www.reddit.com/r/emacs/comments/dfxe1u/codefolding_based_off_indent_level/f370ish/?utm_source=reddit&utm_medium=web2x&context=3
(defun my/toggle-indentation-fold ()
  "Toggle code folding according to indentation of current line."
  (interactive)
  (set-selective-display
   (if selective-display
       nil
     (save-excursion
       (back-to-indentation)
       (1+ (current-column))))))
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

(provide '.emacs)

;;; .emacs ends here
