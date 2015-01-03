(defun eds-util-is-erl-file? (filename)
  (equal (file-name-extension filename) "erl"))

(defun eds-util-get-filename-module-name (filename)
  (file-name-sans-extension (file-name-nondirectory filename)))
