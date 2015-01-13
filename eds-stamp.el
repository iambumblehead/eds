;; stamp .erl files
;;
;; ex:
;; ```
;; %% Filename: chat_app.erl  
;; %% Timestamp: 2015.01.13-12:10:19 (last modified)
;; %% Copyright: mycopy  
;; %% Author(s): me <me@domain.com>  
;; ```
;;
;; stamp is **only** added when,
;;  - buffer file is an erl file
;;  - buffer file is in from src/ of the active project
;;  - st-author or st-copy are defined for the active project
;;
;; when the file is saved,
;;  - if no stamp exists in the buffer file, 
;;    - one is added to the top of that file when it is saved
;;  - if stamp exists in the buffer file
;;    - the the timstamp is updated to the current time when it is saved
;;  - if either st-author or st-copy is undefined
;;    - the undefined is not included in the created stamp 

(defvar *EDS-STAMP-timestamp-spec* 
  "%Y.%m.%d-%H:%M:%S")
(defvar *EDS-STAMP-timestamp-re* 
  "^%.*Timestamp:\[a-Z ]*\\(\[0-9\]+.\[0-9\]+.\[0-9\]+\\)\.*")

(defvar *EDS-STAMP-timestamp-str* 
  "%% Timestamp: :timestamp (last modified)")
(defvar *EDS-STAMP-filename-str* 
  "%% Filename: :filename  ")
(defvar *EDS-STAMP-copyright-str* 
  "%% Copyright: :copyright  ")
(defvar *EDS-STAMP-author-str* 
  "%% Author(s): :author  ")

(defun eds-stamp-get-time ()
  "get the formatted time usable for a timestamp"
  (format-time-string *EDS-STAMP-timestamp-spec*))

(defun eds-stamp-get-filename (&optional file-name) (interactive)
  "get full formatted filename line for the stamp"
  (let ((name (if file-name file-name buffer-file-name)))
    (if name
        (let ((filename (file-name-nondirectory name)))
          (replace-regexp-in-string 
           ":filename" filename *EDS-STAMP-filename-str*))
      (warn "[!!!] eds-stamp: invalid filename"))))

(defun eds-stamp-get-timestamp () (interactive)
  "get full formatted timestamp line for the stamp"
  (replace-regexp-in-string 
   ":timestamp" (eds-stamp-get-time) *EDS-STAMP-timestamp-str*))

(defun eds-stamp-get-author (&optional author) (interactive)
  "get full formatted author line for the stamp"
  (replace-regexp-in-string 
   ":author" (or author (eds-proj-get-st-author)) *EDS-STAMP-author-str*))

(defun eds-stamp-get-copyright (&optional copy) (interactive)
  "get full formatted copyright line for the stamp"
  (replace-regexp-in-string 
   ":copyright" (or copy (eds-proj-get-st-copy)) *EDS-STAMP-copyright-str*))

(defun eds-stamp-full () (interactive)
  "get full stamp at top of buffer file"
  (let ((copy   (eds-proj-get-st-copy))
        (author (eds-proj-get-st-author)))
    (concat 
     (eds-stamp-get-filename) "\n"
     (eds-stamp-get-timestamp) "\n"
     (if copy   (concat (eds-stamp-get-copyright) "\n") "") 
     (if author (concat (eds-stamp-get-author)    "\n") ""))))

(defun eds-stamp-action () (interactive)
  "if the stamp timestamp exists in the buffer file update it, else add a new
stamp to the top of the file."
  (let ((regexp *EDS-STAMP-timestamp-re*)
        (timestamp (eds-stamp-get-timestamp))
        (time (eds-stamp-get-time))
        (old-point (point)))
    (beginning-of-buffer) 
    (if (re-search-forward regexp nil t)
        (if (equal (match-string 1) time)
            (goto-char old-point)
          (print timestamp)
          (replace-match timestamp)
          (goto-char old-point))
      (insert (eds-stamp-full)))))

(defun eds-stamp-action-try () (interactive)
  "if the buffer file is stampable then add stamp"
  (when (eds-proj-is-stampable-file? buffer-file-name)
    (eds-stamp-action)))
  

(add-hook 'write-file-hooks '(lambda () (eds-stamp-action-try) nil))



