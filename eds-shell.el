(defvar *EDS-SHELL-id* 0)

(defun eds-shell-get-id (&optional name) (interactive)
  (concat (eds-proj-get-nodename name) "-"
          (number-to-string (incf *EDS-SHELL-id*))))

(defun eds-shell-get-erl-cmd (node-name &optional name) (interactive)
  (let* ((erl-bin   (eds-proj-get-otppath name))
         (config    (eds-proj-get-configpath name))
         (cmd       (list erl-bin "-sname" node-name)))
    (if config (append cmd (list "-config" (f-no-ext config))) cmd)))

(defun eds-shell-proj (&optional name) (interactive)
  (eds-shell name (eds-proj-get-root-dir) 't))

;; https://github.com/tonini/owl.el
(defun eds-shell (&optional name dir switch-to-buffer) (interactive)
  "Start an interactive erlang shell."
  (edts-api-ensure-server-started)
  (let* ((shell-id    (eds-shell-get-id))
         (buffer-name (format "*edts[%s]*" shell-id))
         (node-name   (format "edts-%s" shell-id))
         (command     (eds-shell-get-erl-cmd node-name))
         (root        (or dir pwd)))
    (let ((buffer (edts-shell-make-comint-buffer
                   buffer-name
                   node-name
                   dir
                   command)))
      (edts-api-init-node-when-ready node-name node-name dir nil)
      (when switch-to-buffer (switch-to-buffer buffer))
      buffer)))

