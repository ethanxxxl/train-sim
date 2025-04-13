(require :sdl2)
(load "./sdl-vector.lisp")

(defun gfx-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Graphics Utility
;; draw either single or multiple lines using vector graphics.

(defvar *line-color* (list 121 219 72 255))
(defun set-line-color (r g b &optional (a 255))
  (setf *line-color* (list r g b a)))

(defun draw-vector-line (renderer x1 y1 x2 y2 &optional (thick 1))
  (let* ((atan-w (atan (- x1 x2) (- y1 y2)))
         (x-offset (- (* thick 0.5 (cos atan-w))))
         (y-offset (* thick 0.5 (sin atan-w)))
         (xa (+ x1 x-offset))
         (ya (+ y1 y-offset))
         (xb (- x1 x-offset))
         (yb (- y1 y-offset))

         (xc (+ x2 x-offset))
         (yc (+ y2 y-offset))
         (xd (- x2 x-offset))
         (yd (- y2 y-offset)))
    
    (sdl2:with-verts ((verts num
                             (list xa ya
                                       xb yb
                                       xc yc
                                       xd yd)
                             *line-color*))
      (sdl2:with-indices ((indices idx-len '(0 1 2
                                             2 3 1)))
        (sdl2:render-geometry renderer nil verts num indices idx-len)))))

(defun draw-vector-lines (renderer thick &rest points)
  "Draws a connected polygon with the provided points.  There must be an even
number of points."
  (assert (mod (/ (length points) 2) 0))

  (loop with last-x = points
        with last-y = (cdr points)
        with x = (cddr last-x)
        with y = (cddr last-y)
        while (or x y)
        do (draw-vector-line renderer
                             (car last-x) (car last-y)
                             (car x) (car y)
                             thick)
           (setf last-x x
                 last-y y
                 x (cddr x)
                 y (cddr y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ngon Utility

(defun ngon-points (x y n rad &optional rot-degrees)
  (let ((rot-radians (* pi (/ (or rot-degrees 0) 180))))
    (loop with ret = (list)
          repeat n
          for theta from rot-radians by (/ (* 2 pi) n)
          do (progn (push (coerce (+ y (* rad (sin theta))) 'single-float) ret)
                    (push (coerce (+ x (* rad (cos theta))) 'single-float) ret))
          finally (return ret))))

(defun ngon-indices (n)
  (loop with indices = (list 0 1 2)
        for i from 3
        repeat (- n 3)
        do (setf indices (append indices (list (- i 1) 0 i)))
        finally (return indices)))

(defun ngon-shell-indices (n)
  (declare (optimize (debug 3)))
  (append (apply 'append
                 (loop for s from 0 below (1- n)
                       collect (list s (1+ s) (+ n s) (1+ s) (+ n s) (+ n s 1))))
          (list (1- n) 0 (+ n n -1) 0 (+ n n -1) n)))

(defun draw-ngon (renderer x y n rad &optional rot-degrees)
  "Draw an ngon at `x',`y'.  It will have `n' sides, with a radius of `rad'.  You
can control where the first point is placed with `rot-degrees'"
  (sdl2:with-verts ((verts num (ngon-points x y n rad rot-degrees) *line-color*))
    (sdl2:with-indices ((indices idx-len (ngon-indices n)))
      (sdl2:render-geometry renderer nil
                            verts num
                            indices idx-len))))

(defun draw-ngon-shell (renderer x y n rad thick &optional rot-degrees)
  "Draws hollowed out ngon at `x', `y'. rad - thickness must be greater than zero"
  (sdl2:with-verts ((verts vert-len
                           (append (ngon-points x y n rad rot-degrees)
                                   (ngon-points x y n (- rad thick) rot-degrees))
                           *line-color*))
    (sdl2:with-indices ((indices idx-len (ngon-shell-indices n)))
      (sdl2:render-geometry renderer nil
                            verts vert-len
                            indices idx-len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphing Utility

(defun draw-sloped-line (renderer m b &optional (thick 2))
  "when m is :inf, then b specifies the x-intercept.  Otherwise, b specifies the
y-intercept according to the equation y = mx + b"
  (let (y1 y2 (x1 0) (x2 800))
    (cond ((and (symbolp m) (equal m :inf))
           (setf y1 0
                 y2 600
                 x1 (+ b 400)
                 x2 (+ b 400)))
          ((numberp m)
           (setf y1 (+ (* (- m) -400) (- b) 300)
                 y2 (+ (* (- m) 400) (- b) 300)))
          (t (error "draw-sloped-line: m must be a number or :inf")))
    
    (draw-vector-line renderer x1 y1 x2 y2 thick)))

(defvar *point-list* nil)
(defvar *mouse-x* 1)
(defvar *mouse-y* 1)
(defvar *ngon* 32)
(defvar *ngon-rad* 100)
(defvar *ngon-rot* 0)
(defun draw-test-graph (renderer)
  ;; draw background grid
  (set-line-color 25 40 25)
  (loop for y from 0 to 600 by 50
        do (draw-vector-line renderer 0 y 800 y 1.0))

    (loop for x from 0 to 800 by 50
          do (draw-vector-line renderer x 0 x 600 1.0))

  ;; origin lines
  (draw-vector-line renderer 0 300 800 300 2)
  (draw-vector-line renderer 400 0 400 600 2)

  ;; draw the lines where the user ctl-clicked.
  (set-line-color 255 255 255)
  (let* ((point-list (reduce 'append *point-list*
                             :key (lambda (p) (list (car p)
                                                    (cdr p)))
                             :initial-value nil))
         (last-point (car (last *point-list*)))
         (last-x (car last-point))
         (last-y (cdr last-point)))
    (when (<= 1 (length *point-list*))
      (draw-ngon renderer last-x last-y 20 5))
    (when (<= 2 (length *point-list*))
      (apply 'draw-vector-lines renderer 1.25 point-list)))

  ;; draw sloped lines that intersect the mouse positions
  (let* ((x (- *mouse-x* 400))
         (y (- (- *mouse-y* 300)))
         (m (if (= x 0) :inf (/ y x)))
         (w (if (= y 0) :inf (- (/ x y))))
         (b-w (if (equal w :inf) x
                  (- y (* w x)))))
    (set-line-color 26 219 122)
    (draw-sloped-line renderer m 0 5)
    
    (set-line-color 26 219 26)
    (draw-sloped-line renderer w b-w 1)

    ;; render a bonus n-gon that is changed with the mouse wheel
    (set-line-color 196 37 39)
    (draw-ngon renderer *mouse-x* *mouse-y*
               *ngon* (* 1 *ngon-rad*) (* 5 (/ *ngon-rot* 45)))
    (set-line-color 255 255 255)
    (draw-ngon-shell renderer *mouse-x* *mouse-y*
                     *ngon* (* 1 *ngon-rad*) (* 0.01 *ngon-rad*) (* 5 (/ *ngon-rot* 45)))))

