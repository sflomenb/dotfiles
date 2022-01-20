(ert-deftest character-to-line ()
  (with-temp-buffer
    (insert "foo")
    (evil-mode 1)
    (evil-yank (point-min) (point-max))
    (my/evil-paste-after-linewise)
    (message "buffer-string: %s" (buffer-string))
    (should (string= (buffer-string) "foo\nfoo"))))

(ert-deftest line-to-character ()
  (with-temp-buffer
    (insert "foo")
    (evil-mode 1)
    (evil-yank (point-min) (point-max) 'line)
    (my/evil-paste-after-characterwise)
    (message "buffer-string: %s" (buffer-string))
    (should (string= (buffer-string) "foofoo"))))
