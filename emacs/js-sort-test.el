(ert-deftest simple ()
  (with-temp-buffer
    (typescript-mode)
    (insert "let foo = { meep: 2, bar: 1 }")
    (goto-char (point-min))
    (search-forward "{")
    (my/sort-js-object)
    (goto-char (point-min))
    (search-forward "{")
    (should (string= (tsc-node-text (tree-sitter-node-at-point 'object))
		     "{ bar: 1, meep: 2 }"))))

(ert-deftest nested ()
  (with-temp-buffer
    (typescript-mode)
    (insert "let foo = {
  bar: 1,
  zeep: {
    z: 1,
    a: 3,
  },
  meep: 2,
}")
    (goto-char (point-min))
    (search-forward "{")

    (my/sort-js-object)

    (goto-char (point-min))
    (search-forward "{")
    (should (string= (tsc-node-text (tree-sitter-node-at-point 'object)) "{
  bar: 1,
  meep: 2,
  zeep: {
    a: 3,
    z: 1,
  },
}"))))

(ert-deftest nested2 ()
  (with-temp-buffer
    (typescript-mode)
    (insert "let foo = {
  bar: 1,
  alala: 'wow',
  zeep: {
    a: {
      y: 'dsfgdsf',
      q: 'whwssdfsdee',
      g: 100,
      w: 'zoop',
    },
    z: 1,
  },
  meep: 2,
}")
    (goto-char (point-min))
    (search-forward "{")

    (my/sort-js-object)

    (goto-char (point-min))
    (search-forward "{")
    (should (string= (tsc-node-text (tree-sitter-node-at-point 'object)) "{
  alala: 'wow',
  bar: 1,
  meep: 2,
  zeep: {
    a: {
      g: 100,
      q: 'whwssdfsdee',
      w: 'zoop',
      y: 'dsfgdsf',
    },
    z: 1,
  },
}"))))
