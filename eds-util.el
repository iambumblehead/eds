
(defun eds-util-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun eds-util-is-erl-file? (filename) (interactive)
  (and filename (equal (file-name-extension filename) "erl")))

(defun eds-util-get-modulename-from-filename (filename) (interactive)
  (if (eds-util-is-erl-file? filename)
      (file-name-sans-extension (file-name-nondirectory filename))
    (warn "[!!!] invalid erlang file")))

(defun eds-util-get-modulename-from-buffer (&optional buffer) (interactive)
  (let ((buffer-filename (buffer-file-name (or buffer (current-buffer)))))
    (eds-util-get-modulename-from-filename buffer-filename)))
