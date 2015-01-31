

(defun eds-dial-analyze-project (&optional n) (interactive)
  "if 'out-plt' does not exist, beam process grows indefinetely to crash OS"
  (let ((node-name (eds-proj-get-nodename n))
        (modules (eds-proj-get-buffer-modulenames n))
        ;;(out-plt (eds-proj-get-base-plt-path n))
        (out-plt (f-join edts-data-directory
                         (concat (eproject-name) ".plt")))
        (otp-plt (eds-proj-get-core-plt-path n)))
    (edts-plugin-call-async node-name
                            'edts_dialyzer
                            'analyze
                            (list (cons "otp_plt" otp-plt)
                                  (cons "out_plt" out-plt)
                                  (cons "modules" modules))
                            #'edts-dialyzer-handle-analysis-result)))  
