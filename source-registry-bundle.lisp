(in-package :cl-user)

(defpackage :source-registry-bundle
  (:use :cl :asdf :alexandria)
  (:export make-bundle))


(in-package :source-registry-bundle)


(defun copy-directory (src dst)
  (let ((src-path (truename (fad:pathname-as-directory src)))
	(dst-path (truename (ensure-directories-exist
			     (fad:pathname-as-directory dst)))))
    (loop for path in (fad:list-directory src-path) do
	 (if (fad:directory-pathname-p path)
	     (let* ((relative (enough-namestring path src-path)))
	       (copy-directory path (fad:merge-pathnames-as-directory dst-path relative)))
	     (fad:copy-file path
			    (fad:merge-pathnames-as-file dst-path
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
                             (fad:merge-pathnames-as-directory
                              dest-path (fad:pathname-as-directory sys-name))))))
