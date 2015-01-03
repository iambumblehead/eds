;;
;; this function taken mostly verbatim from edts source
;;
(defun eds-compile-and-load (module file callback &optional node-name)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name   (or node-name (eds-proj-get-nodename)))
         (resource    (list "nodes" node-name "modules" module))
         (rest-args   (list (cons "file" file)))
         (cb-args     (list callback 201)))
    (print 
     (concat "node-name: " node-name " resource: " module))
    (edts-log-debug "Compiling %s async on %s" module node-name)
    (edts-rest-post-async resource
                          rest-args
                          #'edts-api-async-callback
                          cb-args)))

(defun eds-compile-file (module-file)
  "compile and load a single file"
  (when (eds-util-is-erl-file? module-file)
    (let ((module-name (eds-util-get-filename-module-name module-file))
          (node-name (eds-proj-get-nodename)))
      (eds-compile-and-load 
       module-name module-file 
       '(lambda (comp-res) (print "[...] compile")) node-name))))

(defun eds-compile-dir (module-dir)
  "compile and load src/*erl files for a module"
  (f-files (f-join module-dir "src") 'eds-compile-file))

(defun eds-compile-libs (libs-dir) 
  "compile and load modules libs dirs"
  (f-directories libs-dir 'eds-compile-dir))

(defun eds-compile-projectlibs () (interactive)
  (let ((project-root (eds-proj-get-root-dir)))
    (print project-root)
    (mapcar #'(lambda (lib-dir)
                (let ((full-lib-dir (f-join project-root lib-dir)))
                  (eds-compile-libs full-lib-dir)))
            (eds-proj-get-lib-dirs))))
