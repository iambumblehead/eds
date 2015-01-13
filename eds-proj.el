(defvar *EDS-PROJ* "")
(defvar *EDS-PROJ-otppath* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-config* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-root-dir* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-log-dir* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-src-dir* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-lib-dirs* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-start-cmd* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-st-author* (make-hash-table :test 'equal))
(defvar *EDS-PROJ-st-copy* (make-hash-table :test 'equal))

;; abstracts some boilerplace used for the get and set values to hash tables.
;; values for a project are defined to a key named to that projects name.
;;
;; ex.,
;;   (gethash *EDS-PROJ-log-dir* "sampleproject")
;;   (gethash *EDS-PROJ-log-dir* "otherproject")
(defmacro eds-proj-hget (htable &optional key)
  `(gethash ,(eds-proj-get-name key) ,htable))
(defmacro eds-proj-hset (htable val &optional key)
  `(puthash ,(eds-proj-get-name key) ,val ,htable))

(defun eds-proj-get-root-dir (&optional n) (interactive)
  (expand-file-name (eds-proj-hget *EDS-PROJ-root-dir* n)))
(defun eds-proj-get-root-dir-join (childpath &optional name)
  (f-join (eds-proj-get-root-dir name) childpath))
(defun eds-proj-set-root-dir (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-root-dir* val n))

(defun eds-proj-get-log-dir (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-log-dir* n))
(defun eds-proj-set-log-dir (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-log-dir* val n))

(defun eds-proj-get-src-dir (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-src-dir* n))
(defun eds-proj-get-src-dirpath (&optional n) (interactive)
  (let ((dir (eds-proj-hget *EDS-PROJ-src-dir* n)))
    (when dir (eds-proj-get-root-dir-join dir n))))
(defun eds-proj-set-src-dir (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-src-dir* val n))

(defun eds-proj-get-config (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-config* n))
(defun eds-proj-get-configpath (&optional n) (interactive)
  (let ((config (eds-proj-hget *EDS-PROJ-config* n)))
    (when config (eds-proj-get-root-dir-join config n))))
(defun eds-proj-set-config (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-config* val n))

(defun eds-proj-get-otppath (&optional n) (interactive)
  (or (eds-proj-hget *EDS-PROJ-otppath* n)
      (executable-find "erl")
      (warn "[!!!] eds: erl bin not found")))
(defun eds-proj-set-otppath (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-otppath* val n))

(defun eds-proj-get-lib-dirs (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-lib-dirs* n))
(defun eds-proj-set-lib-dirs (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-lib-dirs* val n))

(defun eds-proj-get-start-cmd (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-start-cmd* n))
(defun eds-proj-set-start-cmd (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-start-cmd* val n))

(defun eds-proj-get-st-author (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-st-author* n))
(defun eds-proj-set-st-author (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-st-author* val n))

(defun eds-proj-get-st-copy (&optional n) (interactive)
  (eds-proj-hget *EDS-PROJ-st-copy* n))
(defun eds-proj-set-st-copy (val &optional n) (interactive)
  (eds-proj-hset *EDS-PROJ-st-copy* val n))

;;;
;;; set / get project names
;;;
(defun eds-proj-get-names () (interactive)
  "print available project names"
  (let ((keys-list '()))
    (maphash '(lambda (key value) 
                (add-to-list 'keys-list key)) *EDS-PROJ-root-dir*)
    (mapconcat 'identity keys-list ", ")))

(defun eds-proj-is-name-valid? (&optional name)
  (when name (equal (stringp name) t)))

(defun eds-proj-get-name (&optional name)
  (or (if (eds-proj-is-name-valid? name) name *EDS-PROJ*) 
      (warn "[!!!] eds: project name undefined")))

(defun eds-proj-set-name (name) (interactive "seds project name: ")
  (setq *EDS-PROJ* name))

(defun eds-proj-get-nodename (&optional name) (interactive)
  "node name -name as an alphanumeric string"
  (replace-regexp-in-string "\\." "" (eds-proj-get-name name)))

;;;
;;; set entire project 
;;;
(defmacro eds-alist-key (alist key)
  `(cdr (assoc ,key ,alist)))

(defun eds-proj-set-opts (name opts) (interactive)
  "set all values specific to the 'name' project"
  (eds-proj-set-name      name)
  (eds-proj-set-otppath   (eds-alist-key opts 'otp-path)  name)
  (eds-proj-set-config    (eds-alist-key opts 'config)    name)
  (eds-proj-set-root-dir  (eds-alist-key opts 'root-dir)  name)
  (eds-proj-set-log-dir   (eds-alist-key opts 'log-dir)   name)
  (eds-proj-set-src-dir   (eds-alist-key opts 'src-dir)   name)
  (eds-proj-set-lib-dirs  (eds-alist-key opts 'lib-dirs)  name)
  (eds-proj-set-start-cmd (eds-alist-key opts 'start-cmd) name)
  (eds-proj-set-st-author (eds-alist-key opts 'st-author) name)
  (eds-proj-set-st-copy   (eds-alist-key opts 'st-copy)   name))

(defun eds-proj-show-opts (&optional name) (interactive)
  (print 
   (concat
    " " (eds-proj-get-name name) ","
    "\n proj-otppath   : " (eds-proj-get-otppath   name)
    "\n proj-config    : " (eds-proj-get-config    name)
    "\n proj-root-dir  : " (eds-proj-get-root-dir  name)
    "\n proj-log-dir   : " (eds-proj-get-log-dir   name)
    "\n proj-src-dir   : " (eds-proj-get-src-dir   name)
    "\n proj-lib-dirs  : " (mapconcat 
                            'identity 
                            (eds-proj-get-lib-dirs name) ", ")
    "\n proj-start-cmd : " (eds-proj-get-start-cmd name) 
    "\n proj-st-author : " (eds-proj-get-st-author name)
    "\n proj-st-copy   : " (eds-proj-get-st-copy   name)
    "\n")))

;;;
;;; proj-specific convenience functions
;;;
(defun eds-proj-is-src-file? (filepath &optional name) (interactive)
  "'true' indicates a filepath in the project src/"
  (let ((full-filepath (expand-file-name filepath))
        (full-srcpath (eds-proj-get-src-dirpath name)))
    (equal (string-match full-srcpath full-filepath) 0)))

(defun eds-proj-is-src-erl-file? (filepath &optional name) (interactive)
  (when (eds-util-is-erl-file? filepath)
    (eds-proj-is-src-file? filepath name)))

(defun eds-proj-is-stampable-file? (filepath &optional name) (interactive)
  "true if filepath in proj src/ and proj includes st-author or st-copy"
  (and (eds-proj-is-src-erl-file? filepath)
       (or (eds-proj-get-st-author)
           (eds-proj-get-st-copy))))

