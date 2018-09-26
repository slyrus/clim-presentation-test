
(cl:defpackage :clim-presentation-test
  (:use #:clim-lisp #:clim)
  (:export #:clim-presentation-test-app-main]))

(in-package :clim-presentation-test)

(define-application-frame clim-presentation-test ()
  ((points :initform nil :accessor points)
   (ink :initform +blue+ :accessor ink))
  (:menu-bar clim-presentation-test-menubar)
  (:panes
   (app :application
        :display-function #'clim-presentation-test-display)
   (interactor :interactor :height 300 :width 600))
  (:layouts
   (default
       (vertically ()
         app
         interactor))))

#+(or)
(define-clim-presentation-test-command (com-refresh
			                :name t
			                :menu t)
    ()
  (window-clear (find-pane-named *application-frame* 'app))
  (clim-presentation-test-display *application-frame*
			          (find-pane-named *application-frame* 'app)))

(defun clim-presentation-test-display (frame pane)
  (with-accessors ((points points)
                   (ink ink))
      frame
    (with-text-size (pane :large)
      ;;
      ;; "Points" Label to start drwaing points
      (with-output-as-presentation
          (t `(com-add-point ,(make-point 0 0)) 'command)
        (format pane "Add Point~&"))
      ;;
      ;; now let's draw the points
      (clim:with-room-for-graphics (pane :first-quadrant nil)
        (let ((ink ink))
          (loop
             :for last-point = nil then point
             :for point :in points
             :do
               (with-output-as-presentation
                   (t point 'point)
                 (clim:draw-circle pane point 6 :ink ink :filled t))
               (when last-point
                 (clim:draw-line pane last-point point :ink ink))))))))

(defun accepting-point (&key (stream *query-io*))
  (accepting-values (stream)
    (let ((x (prog1
                 (accept 'integer :stream stream :default 0 :prompt "Point X: ")
               (fresh-line stream)))
          (y (prog1
                 (accept 'integer :stream stream :default 0 :prompt "Point Y: ")
               (fresh-line stream))))
      (clim:make-point x y))))

(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-clim-presentation-test-command (com-drag-point)
    ((presentation t) (x real) (y real))
  (print (list 'foo presentation x y) *debug-io*)
  (let ((parent (clim:output-record-parent presentation)))
    #+(or) (describe presentation *debug-io*)
    (multiple-value-bind (px py)
        (output-record-position parent)
      (with-accessors ((ink ink))
          *application-frame*
        (let ((pane (get-frame-pane *application-frame* 'app)))
          (multiple-value-bind (x y)
	      (dragging-output (pane :finish-on-release t)
	        (draw-circle pane (get-pointer-position pane) 6
                             :ink ink :filled t))
            (com-move-point (clim:presentation-object presentation)
                            (+ (- x px) 6)
                            (+ (- y py) 6))))))))

(define-presentation-to-command-translator point-dragging-translator
    (t com-drag-point clim-presentation-test
       :tester ((object presentation)
                (declare (ignore presentation))
                (pointp object)))
    (object presentation x y)
  (list presentation x y))

(define-clim-presentation-test-command (com-set-point)
    ((original point) (x real) (y real))
  (declare (ignore original))
  (com-add-point (make-point x y)))

(defun insert-before (new-item before-item list)
  (let ((tail (member before-item list)))
            (if tail
                (progn (rplacd tail (cons (car tail) (cdr tail)))
                       (rplaca tail new-item))
                (push new-item list))))

(define-clim-presentation-test-command (com-add-point)
    ((center point :prompt "point")
     &key (previous-point point :default nil))
  (with-accessors ((points points)
                   (stream frame-standard-input))
      *application-frame*
    (when center
      (if previous-point
          (insert-before center previous-point points)
          (push center points)))
    (loop for point in points
       do (with-output-as-presentation
              (t point 'clim:point)
            (format t "~&~A ~A" (clim:point-x point) (clim:point-y point))))))

(define-clim-presentation-test-command (com-move-point)
    ((point point :prompt "point")
     (x real :prompt "X")
     (y real :prompt "Y"))
  (with-accessors ((points points)
                   (stream frame-standard-input))
      *application-frame*
    (when (and point x y)
      (let ((tail (member point points)))
        (when tail
          (rplaca tail (make-point x y)))))))

(define-presentation-type my-rectangle ()
  :inherit-from 'rectangle)

(defun accepting-rectangle (&key (stream *query-io*))
  (accepting-values (stream)
    (let ((x1 (prog1
                  (accept 'real :stream stream :default 0 :prompt "X1: ")
                (fresh-line stream)))
          (y1 (prog1
                  (accept 'real :stream stream :default 0 :prompt "Y1: ")
                (fresh-line stream)))
          (x2 (prog1
                  (accept 'real :stream stream :default 0 :prompt "X2: ")
                (fresh-line stream)))
          (y2 (prog1
                  (accept 'real :stream stream :default 0 :prompt "Y2: ")
                (fresh-line stream))))
      (clim:make-rectangle* x1 y1 x2 y2))))

(define-clim-presentation-test-command (com-draw-rectangle
                                        :name t
                                        :menu nil)
    ()
  (with-accessors ((points points)
                   (stream frame-standard-input))
      *application-frame*
    (let ((rect (accepting-rectangle :stream stream)))
      (when rect
        (with-output-as-presentation
            (t rect 'my-rectangle)
          (clim:with-bounding-rectangle* (x1 y1 x2 y2) rect
            (clim:with-room-for-graphics (stream)
              (clim:draw-rectangle* stream x1 y1 x2 y2 :ink +blue+))))))))

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

