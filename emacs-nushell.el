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

(defun nu/display-output (output-string)
  (with-current-buffer (get-buffer-create "*nushell-output*")
    (read-only-mode 0)
    (delete-region (point-min) (point-max))
    (let* ((rows (json-read-from-string (base64-decode-string output-string)))
           (rows (if (and (not (arrayp rows))
                          (json-alist-p rows))
                     (vector rows)
                   rows))
           (cols (nu/infer-columns (aref rows 0))))
      (message "COLS: %s" cols)
      (setq tabulated-list-format cols)
      (setq tabulated-list-entries (nu/format-rows rows)))
    (switch-to-buffer (current-buffer))
    (nushell-view-mode)))

(define-derived-mode nushell-view-mode tabulated-list-mode "Nushell Output"
  "Major mode for browsing Nushell result lists."
  :interactive nil
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print))
