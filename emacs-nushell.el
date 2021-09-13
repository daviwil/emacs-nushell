(defun nu/infer-columns (row)
  (apply #'vector
         (mapcar (lambda (col)
                    (list (format "%s" (car col)) 20 t))
                  row)))

(defun nu/prepare-column-value (pair)
  (let ((value (cdr pair)))
    (if (stringp value)
        value
      (propertize (format "%s" value) :foreground "green"))))

(defun nu/format-rows (rows)
  (let ((id -1))
    (incf id)
    (mapcar (lambda (row)
              (list id (apply #'vector (mapcar #'nu/prepare-column-value row))))
            rows)))

(defvar nu/test-input
  "W3sibmFtZSI6IkxJQ0VOU0UiLCJ0eXBlIjoiRmlsZSIsInNpemUiOjEwNjksIm1vZGlmaWVkIjoiMjAyMS0wOS0xMiAyMDozODoyNS4zODc2Njc5OTIgKzAwOjAwIn0seyJuYW1lIjoiVEVTVCIsInR5cGUiOiJGaWxlIiwic2l6ZSI6MCwibW9kaWZpZWQiOiIyMDIxLTA5LTEyIDIxOjI3OjA0LjM0MjI5MTkyMCArMDA6MDAifV0=")

(nu/display-output nu/test-input)
(defun nu/display-alist-vector (vlist)
  "Show VLIST as a table.

VLIST must be a vector where every element is an association list. All
vector elements should be ordered in the same way, e.g.

   [((name11 . value11) (name12 . value12) ...)
    ((name21 . value21) (name22 . value22) ...)
    ((name31 . value31) (name32 . value32) ...)
    ((name41 . value41) (name42 . value42) ...)
    ...]

Also, all names for the Nth entry should be the same, i.e.

   (and
     (eq name11 name21)
     (eq name11 name31)
     (eq name11 name41)
     ...)
should be true.

The display may be corrupted or surprising if the names or the types of
the values differ."
  (unless (vectorp vlist)
    (signal 'wrong-type-argument '(vectorp vlist)))
  (pcase (seq-find (lambda (x) (not (json-alist-p x))) vlist)
    ('nil nil)
    (obj (signal 'wrong-type-argument '(json-alist-p obj))))
  (with-current-buffer (get-buffer-create "*nushell-output*")
    (read-only-mode 0)
    (setq tabulated-list-format (nu/infer-columns (seq-first vlist)))
    (setq tabulated-list-entries (nu/format-rows vlist))
    (tabulated-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defun nu/display-output (data)
  "Display DATA in a table.

DATA must be either
- a vector of association lists
- an association list
- a JSON encoded as a string
- a base64 encoded JSON string.

All other types will lead to an error."
  (pcase data
    ((pred stringp)
     (nu/display-output
      (json-read-from-string
       (or (condition-case nil
               ;; Try base64 decoding first
               (base64-decode-string data)
             (error nil))
           data))))
    ((pred json-alist-p)
     (nu/display-output (vector json-data)))
    ((pred vectorp)
     (nu/display-alist-vector data))
    (_ (signal 'wrong-type-argument nil))))

(define-derived-mode nushell-view-mode tabulated-list-mode "Nushell Output"
  "Major mode for browsing Nushell result lists."
  :interactive nil
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print))
