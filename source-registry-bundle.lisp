(in-package :cl-user)

(defpackage :source-registry-bundle
  (:use :cl :asdf :alexandria)
  (:export make-bundle))


(in-package :source-registry-bundle)


(defun %list-files (components &optional (base-pathname "./"))
  (loop for child in components
     for child-pathname = (component-relative-pathname child) collecting
       (if (subtypep (class-of child) 'file-component)
	   (path:catfile base-pathname child-pathname)
	   (%list-files (component-children child)
			(path:catdir base-pathname child-pathname)))))


(defun list-files (system)
  (flatten (%list-files (component-children system))))


;; this one is too smart
(defun clone-system (system dest-pathspec)
  (let* ((sys-path (component-pathname system))
	 (sys-def-path (system-definition-pathname system))
	 (dest-path (fad:pathname-as-directory dest-pathspec))
	 (sources (list-files system)))
    (when sys-path ; uiop doesn't have a definition file
      (ensure-directories-exist dest-path)
      (fad:copy-file sys-def-path
		     (path:catfile dest-path (file-namestring sys-def-path))
		     :overwrite t)
      (loop for source in sources
	 for src = (path:catfile sys-path source)
	 for dest = (path:catfile dest-path source)
	 do
	   (ensure-directories-exist dest)
	   (fad:copy-file src dest :overwrite t)
	 finally (return t)))))


(defun copy-directory (src dst)
  (let ((src-path (truename (fad:pathname-as-directory src)))
	(dst-path (truename (ensure-directories-exist
			     (fad:pathname-as-directory dst)))))
    (loop for path in (fad:list-directory src-path) do
	 (if (fad:directory-pathname-p path)
	     (let* ((relative (enough-namestring path src-path)))
	       (copy-directory path (path:catdir dst-path relative)))
	     (fad:copy-file path
			    (path:catfile dst-path
					  (file-namestring path)))))))


(defun brute-clone-system (system dest-pathspec)
  (let* ((sys-path (fad:pathname-directory-pathname (system-definition-pathname system)))
	 (dest-path (fad:pathname-as-directory dest-pathspec)))
    (when (and sys-path               ; uiop doesn't have a definition file
               (> (length (string-trim '(#\Space #\Tab #\Newline) (namestring sys-path))) 0))
      (copy-directory sys-path dest-path))))


(defun extract-name (sys-def)
  (if (listp sys-def)
      (ecase (first sys-def)
        (:version (second sys-def)))
      sys-def))

(defun %list-dependencies (system)
  (let ((dependencies (mapcar #'extract-name (system-depends-on system))))
    (nconc
     (loop for dependency in dependencies append
          (list-dependencies (find-system dependency)))
     dependencies)))

(defun list-dependencies (system)
  (delete-duplicates (%list-dependencies system) :test 'equal))


(defun make-bundle (system-designator dest-pathspec)
  (let ((system (find-system system-designator))
	(dest-path (fad:pathname-as-directory dest-pathspec)))
    (loop for sys-name in (cons (component-name system) (list-dependencies system)) do
	 (brute-clone-system (find-system sys-name)
		       (path:catdir dest-path (fad:pathname-as-directory sys-name))))))
