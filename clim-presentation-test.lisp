
(cl:defpackage :clim-presentation-test
  (:use #:clim-lisp #:clim)
  (:export #:clim-presentation-test))

(in-package :clim-presentation-test)

(define-application-frame clim-presentation-test ()
  ((warp-level :initform 0 :accessor warp-level))
  (:menu-bar clim-presentation-test-menubar)
  (:panes
   (app :application
        :display-function #'clim-presentation-test-display)
   (interactor :interactor :height 200 :width 600))
  (:layouts
   (default
       (vertically ()
         app
         interactor))))

(define-presentation-type warp-level ()
  :inherit-from 'integer)

(defun clim-presentation-test-display (frame pane)
  (with-accessors ((warp-level warp-level))
      frame
    (with-text-size (pane :large)
      (fresh-line pane)
      (with-output-as-presentation
          (t warp-level 'warp-level)
        (format t "~A" warp-level)))))

(defun accepting-warp-level (&key (stream *query-io*))
  (accepting-values (stream)
    (accept 'integer
            :stream stream
            :default 4
            :prompt "Warp Level")))

(define-clim-presentation-test-command (com-set-warp-level
                                        :name t
                                        :menu nil)
    ()
  (with-accessors ((warp-level warp-level)
                   (stream frame-standard-input))
      *application-frame*
    (setf warp-level (accepting-warp-level :stream stream))
    (format stream "New Warp Level: ~S~%" warp-level)))

(define-clim-presentation-test-command (com-quit :name t :menu "Quit")
   ()
  (frame-exit *application-frame*))

(make-command-table 'clim-presentation-test-file-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'clim-presentation-test-menubar
		    :errorp nil
		    :menu '(("File" :menu clim-presentation-test-file-command-table)))

(defvar *clim-presentation-test-app*)

(defun clim-presentation-test-app-main (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'clim-presentation-test)))
             (setf *clim-presentation-test-app* frame)
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "CLIM Presentation Test App")
        (run))))

