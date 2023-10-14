;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2004, Jochen Schmidt <js@codeartist.org>.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :mel.folders.maildir)

;;;;;;;;;;;;;;;;;;;;;;
;; Maildir handling ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Dependencies
;; Encapsulating Streams
;; (MEL.UNIX:DIRECTORY-CONTENTS-CHANGED-P)
;; (MEL.FILESYSTEM:MAP-DIRECTORY)
;; MEL.UNIX:UNIVERSAL-TO-UNIX-TIME (portable)
;; MEL.ENVIRONMENT:GETPID (workaround portable)
;; MEL.ENVIRONMENT:GETHOSTNAME (workaround portable)
;; FILE-DIRECTORY-P

;;; An input-message-stream provider...
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass body-start-cache-mixin (folder)
  ((body-start-cache :accessor body-start-cache 
		       :initform (make-hash-table :test 'equal))))
)

(defclass maildir-folder (basic-receiver basic-sender)
  ((current-mail :accessor current-mail :type pathname :initarg :current-mail)
   (new-mail :accessor new-mail :type pathname :initarg :new-mail)
   (temporary-mail :accessor temporary-mail :type pathname :initarg :temporary-mail)
   (uid-cache :accessor uid-cache :initform (make-hash-table :test 'equal))

   (new-mail-counter :initform 0)
   (selected-messages :accessor selected-messages :initform nil)
   (recent-messages)
   (change-tag :initform nil)
   (line-terminator :initform nil :initarg :line-terminator-style :reader line-terminator-style)
   (current-unique-name :initform nil :accessor current-unique-name)))

(defmethod short-name ((folder maildir-folder))
  (concatenate 'string "maildir://" (first (last (pathname-directory (name folder))))))

;; Protocol

(defmethod initialize-instance :after ((folder maildir-folder) 
				       &key (if-does-not-exist :error)
				       &allow-other-keys)
  (let* ((pathname (mel.filesystem:directory-pathname (name folder))))
    (setf (current-mail folder) (merge-pathnames (make-pathname :directory `(:relative "cur")) pathname)
	  (new-mail folder) (merge-pathnames (make-pathname :directory `(:relative "new")) pathname)
	  (temporary-mail folder) (merge-pathnames (make-pathname :directory `(:relative "tmp")) pathname))

    (unless (and (file-directory-p pathname)
                 (file-directory-p (current-mail folder))
                 (file-directory-p (new-mail folder))
                 (file-directory-p (temporary-mail folder)))
      (case if-does-not-exist
        (:error (error "Maildir-Folder ~A does not exist" pathname))
        (:create (ensure-directories-exist (print(current-mail folder)) :verbose t)
                 (ensure-directories-exist (new-mail folder) :verbose t)
                 (ensure-directories-exist (temporary-mail folder) :verbose t))))
    (let ((*default-pathname-defaults* (pathname "")))
      (setf (current-mail folder) (truename (current-mail folder))
            (new-mail folder) (truename (new-mail folder))
            (temporary-mail folder) (truename (temporary-mail folder))))
    folder))

(defmethod make-maildir-folder (pathname &key (if-does-not-exist :error) (line-terminator-style nil) &allow-other-keys)
  (make-instance 'maildir-folder :name pathname :if-does-not-exist if-does-not-exist :line-terminator-style line-terminator-style))

#+nil
(defmethod make-maildir-folder (pathname &rest args &key (if-does-not-exist :error) (line-terminator-style nil) &allow-other-keys)
  (declare (ignore args))
  (let* ((pathname (mel.filesystem:directory-pathname pathname))
         (folder (make-instance 'maildir-folder
                               :name (namestring pathname)
			       :line-terminator-style line-terminator-style
                               :current-mail (merge-pathnames (make-pathname :directory `(:relative "cur")) pathname)
                               :new-mail (merge-pathnames (make-pathname :directory `(:relative "new")) pathname)
                               :temporary-mail (merge-pathnames (make-pathname :directory `(:relative "tmp")) pathname))))
    (unless (and (file-directory-p pathname)
                 (file-directory-p (current-mail folder))
                 (file-directory-p (new-mail folder))
                 (file-directory-p (temporary-mail folder)))
      (case if-does-not-exist
        (:error (error "Maildir-Folder ~A does not exist" pathname))
        (:create (ensure-directories-exist (print(current-mail folder)) :verbose t)
                 (ensure-directories-exist (new-mail folder) :verbose t)
                 (ensure-directories-exist (temporary-mail folder) :verbose t))))
    (let ((*default-pathname-defaults* (pathname "")))
      (setf (current-mail folder) (truename (current-mail folder))
            (new-mail folder) (truename (new-mail folder))
            (temporary-mail folder) (truename (temporary-mail folder))))
    folder))

(defmethod serialize-folder ((folder maildir-folder) stream)
  (with-standard-io-syntax 
    (write `(make-maildir-folder ,(name folder))
	   :stream stream)))

(defmethod close-folder ((folder maildir-folder)))

(defclass maildir-message-storing-stream (encapsulating-output-stream)
  ((unique-message-name :accessor unique-message-name :initarg :unique-message-name)
   (message :accessor message :initarg :message)
   (folder :accessor stream-folder :initarg :folder)))

(defclass maildir-message-input-stream (encapsulating-input-stream) ())

(defmethod close ((stream maildir-message-storing-stream) &key abort)
  (close (encapsulated-output-stream stream) :abort abort)
  (unless abort
    
    (let ((from (mel.filesystem:append-name (temporary-mail (stream-folder stream))  (unique-message-name stream)))
	  (to  (mel.filesystem:append-name (new-mail (stream-folder stream)) (unique-message-name stream))))
      (rename-file from to))))

(defmethod close ((stream maildir-message-input-stream) &key abort)
  (close (encapsulated-input-stream stream) :abort abort)
  (unless abort
    (let ((path (truename (encapsulated-input-stream stream))))
      (rename-file path (truename (merge-pathnames (make-pathname :directory '(:relative :up "cur")) path)))
      nil)))

(defmethod open-message-storing-stream ((folder maildir-folder) message)
  (let* ((unique-name (unique-name folder))
         (file (make-pathname :name unique-name :defaults (temporary-mail folder))))
    (let ((stream (make-instance 'maildir-message-storing-stream
				 :output-stream (open file 
						      :external-format #-lispworks :default #+lispworks '(:latin-1 :eol-style :lf) ;faithful output
						      :direction :output
						      :if-exists :error
						      :if-does-not-exist :create)
				 :unique-message-name unique-name
				 :message message
				 :folder folder)))
      (case (line-terminator-style folder)
	(:rfc (make-instance 'mel.internal::rfc-line-terminator-output-stream
			     :output-stream stream))
	(:unix (make-instance 'mel.internal::unix-line-terminator-output-stream
			      :output-stream stream))
	(:mac (make-instance 'mel.internal::rfc-line-terminator-output-stream
			     :output-stream stream))
	(otherwise stream)))))

;; Status info tag

(defun generate-tag (&rest keys)
  (with-output-to-string (s)
    (flet ((emit-option (key)
             (when (getf keys key)
               (write-char (ecase key
                             (:replied #\R)
                             (:seen #\S)
                             (:trashed #\T)
                             (:draft #\D)
                             (:flagged #\F))
                           s))))
    
      (write-string "2," s)
      (emit-option :draft)
      (emit-option :flagged)
      (emit-option :replied)
      (emit-option :seen)
      (emit-option :trashed))))

(defun merge-tags (&rest keys &key (defaults nil) &allow-other-keys)
  (let ((merged-tag (copy-seq defaults)))
    (loop for (key value) on keys by #'cddr
          do (unless (eq key :defaults)
               (setf (getf merged-tag key) value)))
    merged-tag))

(defun parse-tags (tags)
  (let ((start (+ 2 (search "2," tags))))
    (mapcan (lambda (tag) (list tag t))
            (loop for i from start below (length tags)
                  for tag = (case (char tags i)
                              (#\R :replied)
                              (#\S :seen)
                              (#\T :trashed)
                              (#\D :draft)
                              (#\F :flagged)
                              (otherwise nil))
                  while tag collect tag))))

(declaim (inline uidify))
(defun uidify (file)
  (declare (optimize (speed 3) (safety 0))
           (type string file))
  (let ((uid-end (position #\: file :from-end t)))
    (if uid-end
      (subseq file 0 uid-end)
      file)))
        
;; intern

(defun find-message-file (folder uid)
  (declare (type string uid)
           (optimize (speed 3) (safety 0)))
  (let ((uid (uidify uid)))
    (declare (type string uid))
    (let ((cell (gethash uid (uid-cache folder))))
      (when cell
        (case (car cell)
	  (:new (mel.filesystem:append-name (new-mail folder) (cdr cell)))
	  (:cur (mel.filesystem:append-name (current-mail folder) (cdr cell))))))))

#|
  (let ((name (let ((uid-end (position #\: uid :from-end t)))
                (or (and (not uid-end) (probe-file (make-pathname :name uid :defaults (new-mail folder))))
                    (find uid (directory (make-pathname :name :wild :type :wild 
                                                        :defaults (current-mail folder)) :directories nil)
                          :key (lambda (file) (enough-namestring file (current-mail folder))) 
                          :test (lambda (uid name) (string= uid name 
                                                            :end1 (or uid-end (length uid))
                                                            :end2 (or uid-end (length uid)))))))))
    (values name
            (let ((uid-end (position #\: (pathname-name name) :from-end t)))
              (when uid-end
                (parse-tags (subseq (pathname-name name) (1+ uid-end))))))))
|#
(defmethod open-message-input-stream-using-folder ((folder maildir-folder) (message message) start)
  (multiple-value-bind (file #+nil tags) (find-message-file folder (uid message))

    (unless file (error 'message-not-found :folder folder :uid (uid message)))
    
    #+nil
    (unless (getf tags :seen) ; message is either in new or unseen in cur
      (let ((tags (merge-tags :seen (not only-header) ; If body not touched set message unseen
                              :defaults tags)))
        (rename-file (mel.filesystem:append-name (new-mail folder) (unique-message-name stream))
                     (setf file (mel.filesystem:append-name (current-mail folder)
							    (if tags (format nil "~A:~A" uid tags) uid))))))

    
    (let ((stream (open file :direction :input :if-does-not-exist :error 
			:external-format #-lispworks :default #+lispworks '(:latin-1 :eol-style :lf))))
      (unless (zerop start) (file-position stream start))
      stream)))

;; This method must be called before any stream-providers!
;; So we specialize it on the class itself because other mixins can then
;; only come later.
(defmethod message-body-stream-using-folder :around ((folder maildir-folder) (message message))
  (let* ((stream (call-next-method))
	(path (truename stream)))
    ;; If message is still in "new" move it to "cur" now
    (unless (equal (car (last (pathname-directory path))) "cur")
      (let* ((name (enough-namestring path (new-mail folder)))
             (new-path (merge-pathnames name (current-mail folder))))
        (rename-file path new-path)
        (setf (gethash name (uid-cache folder)) (cons :cur name))))
    stream))

#+nil
(defmethod message-body-stream-using-folder :around ((folder body-start-cache-mixin) (message message))
  (let (stream)
    (multiple-value-bind (body-start existsp)
	(gethash (uid message) (body-start-cache folder))
      (cond
       ;; Restore body-start position
       (existsp (setf stream (open-message-input-stream message body-start)))
       ;; Find body-start position
       (t (setf stream (call-next-method))
	  ;; Cache header start position for later
	  (setf (gethash (uid message) (body-start-cache folder)) 
		(file-position stream))))
      stream)))

(defmethod delete-message-using-folder ((folder maildir-folder) (message message))
  (let ((file (find-message-file folder (uid message))))
    (remhash (uid message) (uid-cache folder))
    (remhash (uid message) (message-cache folder))
    (setf (selected-messages folder) (delete message (selected-messages folder)))
    (if (probe-file file)
      (delete-file file)
      (error "Message does not exist"))))

(defmethod move-message-using-folders 
  ((message message) (message-folder maildir-folder) (sink-folder maildir-folder))
  (let ((file (find-message-file message-folder (uid message))))
    (remhash (uid message) (uid-cache message-folder))
    (remhash (uid message) (message-cache message-folder))
    (setf (selected-messages message-folder) (delete message (selected-messages message-folder)))
    (let* ((unique-name (unique-name sink-folder))
           (sink-file (mel.filesystem:append-name (new-mail sink-folder)  unique-name)))
      (if (probe-file file)
	  (rename-file file sink-file)
	(error "Message does not exist"))
      ;; Register message-object with new folder
      (setf (uid message) unique-name    ; general
	    (folder message) sink-folder ; general
	    (gethash unique-name (uid-cache sink-folder)) (cons 
							   :new 
							   (mel.filesystem:only-name-and-type
							    sink-file)))
      (push message (selected-messages sink-folder))
      )))

(defmethod message-size-using-folder ((folder maildir-folder) (message message))
  (let ((file (find-message-file folder (uid message))))
    (with-open-file (file file)
		    (file-length file))))

#+obsolete?
(defmethod collect-headers ((folder maildir-folder))
  (let ((headers nil))
    (map-selected-messages folder
                           (lambda (message-stream)
                             (push (read-rfc2822-header message-stream)
                                   headers)))
    (nreverse headers)))

(defun folder-recent-p (folder)
  (with-slots (change-tag) folder
    (not (when-let (new-change-tag (directory-contents-changed-p 
				    (new-mail folder)
				    change-tag))
		   (setf change-tag new-change-tag)))))

(defmethod map-recent-messages (fn (folder maildir-folder))
  (or (and (folder-recent-p folder)
	   (slot-boundp folder 'recent-messages)
	   (map nil fn (slot-value folder 'recent-messages)))
      (setf (slot-value folder 'recent-messages)
	    (let ((messages nil))
	      (flet ((push-message (file)
		       (let ((message (find-message folder file :if-does-not-exist :create)))
			 (push message messages)
			 message)))
		(declare #-(or sbcl cmu)(dynamic-extent #'push-message))
		(let ((uid-cache (uid-cache folder)))

		  (mel.filesystem:map-directory 
		   (lambda (file)
		     (setf (gethash file uid-cache) (cons :new file))
		     (funcall fn (push-message file)))
		   (namestring (truename (new-mail folder))))))

	      (nreverse messages)))))

(defmethod flagp-using-folder ((folder maildir-folder) message (flag (eql :recent)))
  (multiple-value-bind (cell existsp)
      (gethash (uidify (uid message)) (uid-cache folder))
    (unless existsp (error "Message does not exist in uid cache"))
    (ecase (car cell)
      (:new t)
      (:cur nil))))

(defmethod mark-message-using-folder ((folder maildir-folder) message (flag (eql :recent)))
  (unless (flagp message :recent)
    (let* ((pathname (find-message-file folder (uid message)))
	   (new-pathname (make-pathname :directory '(:relative :back "new")
					:defaults pathname)))
      (format t "Rename from ~A to ~A~%" (namestring pathname) new-pathname)
      (rename-file (namestring pathname) new-pathname)
      (remhash (uidify (uid message)) (uid-cache folder))
      (setf (gethash (uidify (uid message)) (uid-cache folder)) (cons
							:new
							(mel.filesystem:only-name-and-type pathname))))))

(defmethod unmark-message-using-folder ((folder maildir-folder) message (flag (eql :recent)))
  (when (flagp message :recent)
    (let* ((pathname (find-message-file folder (uid message)))
	  (new-pathname (make-pathname :directory '(:relative :back "cur")
					:defaults pathname)))
      (format t "Rename from ~A to ~A~%" (namestring pathname) new-pathname)
      (rename-file (namestring pathname) new-pathname)
      (remhash (uidify (uid message)) (uid-cache folder))
      (setf (gethash (uidify (uid message)) (uid-cache folder)) (cons
							:cur
							(mel.filesystem:only-name-and-type pathname))))))

(defmethod map-messages (fn (folder maildir-folder))
  (declare (optimize (speed 0) (safety 3)))
  (or (and (folder-recent-p folder)
	   (slot-boundp folder 'selected-messages)
	   (selected-messages folder)
	   (progn (map nil fn (selected-messages folder)) (selected-messages folder)))
      (setf (selected-messages folder)
	    (let ((messages nil))
	      (flet ((push-message (file)
		       (let ((message (find-message folder file :if-does-not-exist :create)))
			 (push message messages)
			 message)))
		(declare #-(or sbcl cmu)(dynamic-extent #'push-message))
		(let ((uid-cache (uid-cache folder)))
                  (dolist (folder-spec (list (list :new (new-mail folder))
                                             (list :cur (current-mail folder))))
                    (destructuring-bind (symbol subfolder) folder-spec
                      (mel.filesystem:map-directory
                       (lambda (file)
                         (declare (type string file))
                         (let ((uid (uidify file)))
                           (setf (gethash uid uid-cache) (cons symbol file))
                           (funcall fn (push-message file))))
                       (namestring (truename subfolder)))))))
	      
	      (nreverse messages)))))

(defun unique-name (folder)
  (let ((unix-time (princ-to-string (universal-to-unix-time
				     (get-universal-time))))
	(pid (princ-to-string (getpid)))
	(counter (princ-to-string (incf (slot-value folder 'new-mail-counter))))
	(hostname (or (gethostname) "localhost")))
    (concatenate 'string
		 unix-time "." pid "_" counter "." hostname)))