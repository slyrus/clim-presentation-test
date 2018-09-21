
(cl:defpackage :clim-presentation-test
  (:use #:clim-lisp #:clim)
  (:export #:clim-presentation-test-app-main]))

(in-package :clim-presentation-test)

(define-application-frame clim-presentation-test ()
  ((warp-level :initform 0 :accessor warp-level)
   (points :initform nil :accessor points))
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
  (with-accessors ((warp-level warp-level)
                   (points points))
      frame
    (with-text-size (pane :large)
      (fresh-line pane)
      (present warp-level 'real)
      (loop for point in points
       do (with-output-as-presentation
              (t point 'clim:point)
            (format t "~&~A ~A" (clim:point-x point) (clim:point-y point)))))))

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
    (with-output-as-presentation
          (t warp-level 'warp-level)
      (format stream "New Warp Level: ~S~%" warp-level))))

(defun accepting-point (&key (stream *query-io*))
  (accepting-values (stream)
    (let ((x (accept 'integer
                     :stream stream
                     :default 0
                     :prompt "Point X: "))
          (y (accept 'integer
                     :stream stream
                     :default 0
                     :prompt "Point Y: ")))
      (clim:make-point x y))))

(define-clim-presentation-test-command (com-add-point
                                        :name t
                                        :menu nil)
    ()
  (with-accessors ((points points)
                   (stream frame-standard-input))
      *application-frame*
    (let ((point (accepting-point :stream stream)))
      (when point
        (push point points)))
    (loop for point in points
       do (with-output-as-presentation
              (t point 'clim:point)
            (format t "~&~A ~A" (clim:point-x point) (clim:point-y point))))))

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

