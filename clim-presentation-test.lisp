
(cl:defpackage :clim-presentation-test
  (:use #:clim-lisp #:clim)
  (:export #:clim-presentation-test-app-main]))

(in-package :clim-presentation-test)

(define-application-frame clim-presentation-test ()
  ((points :initform nil :accessor points)
   (ink :initform +blue+ :accessor ink)
   (view-origin :initform nil :accessor view-origin))
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

;; 0. create a CLOS subclass for line output records
(defclass line-output-record (standard-presentation) ())

;; 1. create a presentation class for line output records
(define-presentation-type line-output-record ()
  :inherit-from 'line)

;; 2. make our present code use this class
(defun clim-presentation-test-display (frame pane)
  (with-accessors ((points points)
                   (ink ink)
                   (view-origin view-origin))
      frame
    (with-text-size (pane :large)
      ;;
      ;; "Points" Label to start drwaing points
      (with-output-as-presentation
          (t `(com-add-point nil 0 0) 'command)
        (format pane "Add Point~&"))
      ;;
      ;; now let's draw the points
      (let ((record
             (with-room-for-graphics (pane :first-quadrant nil)
               (loop
                  :for last-point = nil then point
                  :for point :in points
                  :do
                    (with-output-as-presentation
                        (t point 'point)
                      (draw-circle pane point 6 :ink ink :filled t))
                    (when last-point
                      (with-output-as-presentation
                          (t (list last-point point) 'line :record-type 'line-output-record)
                        (draw-line pane last-point point :ink ink))
                      *debug-io*)))))
        (unless view-origin
          (multiple-value-bind (px py)
              (output-record-position record)
            (setf view-origin (make-point px py))))))))

(defun point-distance (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (sqrt (+ (* (- x2 x1) (- x2 x1))
               (* (- y2 y1) (- y2 y1)))))))

(defun line-point-between-p (test-point line-point-1 line-point-2
                                 &key (line-fuzz 0.5d0))
  (let ((d1 (point-distance test-point line-point-1))
        (d2 (point-distance test-point line-point-2))
        (line-length (point-distance line-point-1 line-point-2)))
    (values (< (abs (- line-length (+ d1 d2)))
               line-fuzz)
            (abs (- line-length (+ d1 d2))))))

;; 3. add a new output-record-refined-position-test method that
;; specializes on this class
(defmethod output-record-refined-position-test ((record line-output-record) x y)
  (print record *debug-io*)
  (let ((line (presentation-object record)))
    (destructuring-bind (p1 p2)
        line
      (line-point-between-p (make-point x y) p1 p2))))

(defun get-pointer-position (pane)
  "Returns a point with x and y values of the stream-pointer-position
of pane."
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-clim-presentation-test-command (com-move-point)
    ((point point :prompt "point")
     (x real :prompt "X")
     (y real :prompt "Y"))
  (with-accessors ((points points))
      *application-frame*
    (when (and point x y)
      (let ((tail (member point points)))
        (when tail
          (rplaca tail (make-point x y)))))))

(define-clim-presentation-test-command (com-drag-move-point)
    ((presentation t))
  (multiple-value-bind (px py)
      (point-position (view-origin *application-frame*))
    (with-accessors ((ink ink))
        *application-frame*
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (x y)
	    (dragging-output (pane :finish-on-release t)
	      (draw-circle pane (get-pointer-position pane) 6
                           :ink ink :filled t))
          (let ((old-point (presentation-object presentation)))
            (com-move-point old-point (- x px) (- y py))))))))

(defun insert-before (new-item before-item list)
  "If before-item is a member of list, inserts new-item in list
 immediately before new-item, otherwise new-item is prepended to the
 beginning of list. Returns the (destructively) modified list."
  (let ((tail (member before-item list)))
            (if tail
                (progn (rplacd tail (cons (car tail) (cdr tail)))
                       (rplaca tail new-item))
                (push new-item list)))
  list)

(defun insert-after (new-item after-item list)
  "If after-item is a member of list Inserts new-item in list
immediately after new-item, otherwise it appends new-item to the end
of list. Returns the (destructively) modified list."
  (let ((tail (member after-item list)))
            (if tail
                (rplacd tail (cons new-item (cdr tail)))
                (append list (list new-item))))
  list)

(define-clim-presentation-test-command (com-add-point)
    ((previous-point point :prompt "point")
     (x real :prompt "X")
     (y real :prompt "Y"))
  (with-accessors ((points points))
      *application-frame*
    (when (and x y)
      (let ((point (make-point x y)))
        (if previous-point
            (insert-before point previous-point points)
            (push point points))))))

(define-clim-presentation-test-command (com-drag-add-point)
    ((old-point t))
  (multiple-value-bind (px py)
      (point-position (view-origin *application-frame*))
    (with-accessors ((ink ink))
        *application-frame*
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (x y)
	    (dragging-output (pane :finish-on-release t)
	      (draw-circle pane (get-pointer-position pane) 6
                           :ink ink :filled t))
          (com-add-point old-point (- x px) (- y py)))))))

(define-gesture-name add-point-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator point-dragging-add-translator
    (point com-drag-add-point clim-presentation-test
           :gesture add-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (pointp object)))
    (object)
  (list object))

(define-gesture-name move-point-gesture :pointer-button (:left))

(define-presentation-to-command-translator point-dragging-move-translator
    (point com-drag-move-point clim-presentation-test
           :gesture move-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (pointp object)))
    (object presentation)
  (list presentation))

(define-clim-presentation-test-command (com-split-line)
    ((presentation t))
  (with-accessors ((points points))
      *application-frame*
    (let ((line-points (presentation-object presentation)))
      (destructuring-bind (p1 p2)
          line-points
        (let ((i1 (position p1 points))
              (i2 (position p2 points)))
          (let ((index (min (or i1 (length points))
                            (or i2 (length points)))))
            (com-drag-add-point (elt points (1+ index)))))))))

(define-gesture-name click-line-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator click-line-translator
    (line com-split-line clim-presentation-test
          :gesture click-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))

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

