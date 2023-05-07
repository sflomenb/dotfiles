;;; Code:
(setq package-enable-at-startup nil)

;; https://blog.d46.us/advanced-emacs-startup/
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; (message "increasing gc-cons-threshold from %d to %d" gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-threshold (* 50 1000 1000))

(provide 'early-init)
;;; early-init.el ends here
