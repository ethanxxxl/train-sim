(require :sdl2)
(require :sdl2-gfx)
(load "./sdl-vector.lisp")

(defstruct station
           "describes a train station that trains may connect to."
  name
  x y)

(defun draw-lines (renderer &rest points)
  "Draws a connected polygon with the provided points.  There must be an even
number of points."
  (assert (mod (/ (length points) 2) 0))

  (let (sdl-points num-points)
    (setf sdl-points 
          (loop with x = points
                with y = (cdr points)
                while (or x y)
                collect (sdl2:make-point (car x) (car y))
                do (setf x (cddr x)
                         y (cddr y))))

    (setf num-points (length sdl-points))

    (sdl2:render-draw-lines renderer
                            (apply #'sdl2:points* sdl-points)
                            num-points)))

(defun draw-station (renderer station)
  "Draws the station onto the screen"
  (let ((x (station-x station))
        (y (station-y station)))
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (draw-lines renderer
                  x (- y 5)
                  (- x 5) (+ y 5)
                  (+ x 5) (+ y 5)
                  x (- y 5))))

(defun draw-train ())

(defun draw-car (renderer car))
(defun draw-engine (renderer engine))

(defun draw-track ())

(defun gfx-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defvar *line-color* (list 121 219 72 255))
(defun set-line-color (r g b &optional (a 255))
  (setf *line-color* (list r g b a)))

(defvar *print-p* nil)
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
  (sdl2:with-verts ((verts num (ngon-points x y n rad rot-degrees) *line-color*))
    (sdl2:with-indices ((indices idx-len (ngon-indices n)))
      (sdl2:render-geometry renderer nil
                            verts num
                            indices idx-len))))

(defun draw-ngon-shell (renderer x y n rad thick &optional rot-degrees)
  "rad - thickness must be greater than zero"
  (sdl2:with-verts ((verts vert-len
                           (append (ngon-points x y n rad rot-degrees)
                                   (ngon-points x y n (- rad thick) rot-degrees))
                           *line-color*))
    (sdl2:with-indices ((indices idx-len (ngon-shell-indices n)))
      (sdl2:render-geometry renderer nil
                            verts vert-len
                            indices idx-len))))

(defun draw-sloped-line (renderer m b &optional (thick 2))
  (let ((y1 (+ (* (- m) -400) (- b) 300))
        (y2 (+ (* (- m) 400) (- b) 300)))
    (draw-vector-line renderer 0 y1 800 y2 thick)))

(defvar *mouse-x* 1)
(defvar *mouse-y* 1)
(defvar *ngon* 32)
(defvar *ngon-rad* 100)
(defvar *ngon-rot* 0)
(defun draw-test-object (renderer)
  (set-line-color 25 40 25)
  (loop for y from 0 to 600 by 50
        do (draw-vector-line renderer 0 y 800 y 1.0))

    (loop for x from 0 to 800 by 50
          do (draw-vector-line renderer x 0 x 600 1.0))

  (draw-vector-line renderer 0 300 800 300 2)
  (draw-vector-line renderer 400 0 400 600 2)

  (let ((x (- *mouse-x* 400))
        (y (- (- *mouse-y* 300))))
    (if (and (/= x 0) (/= y 0))
        (let* ((m (/ y x))
               (w (- (/ x y))))

          (set-line-color 26 219 122)
          (draw-sloped-line renderer m 0 10)
          
          (set-line-color 26 219 26)
          (draw-sloped-line renderer w (- y (* w x)) 1)

          (set-line-color 247 17 18)
          (draw-ngon-shell renderer *mouse-x* *mouse-y* *ngon* (* 10 *ngon-rad*) 1 (* 5 (/ *ngon-rot* 45)))))))


(defvar *run-program* nil
  "set to true during main function, when threads are created and graphics is
started.")

(defvar *renderer* nil "variable for debugging purposes")
(defvar +screen-width+ 640)
(defvar +screen-height+ 480)
(defvar *ctrl-key* nil "set to true while ctrl key is pressed")
(defvar *shift-key* nil "set to true while shift key is pressed")

(defun gfx-loop-thread ()
"use SDL to create a graphics window and draw stuff"
  (let ((station (make-station :name "test-station" :x 50 :y 50)))
    ;; initialization
    (sdl2:with-init (:everything)
      ;; enable Anti-Aliasing
      (sdl2-ffi.functions:sdl-gl-set-attribute
       sdl2-ffi:+sdl-gl-multisamplesamples+ 4)
      
      (sdl2:with-window (win :title "SDL2 from Common Lisp" :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (setf *renderer* renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (alexandria:switch ((sdl2:scancode-value keysym)
                                 :test 'sdl2:scancode=)
               (:scancode-lctrl (setf *ctrl-key* t))
               (:scancode-lshift (setf *shift-key* t))))
            (:keyup
             (:keysym keysym)
             (alexandria:switch ((sdl2:scancode-value keysym)
                                 :test 'sdl2:scancode=)
               (:scancode-escape (sdl2:push-event :quit))
               (:scancode-lctrl (setf *ctrl-key* nil))
               (:scancode-lshift (setf *shift-key* nil))))
            (:mousebuttondown ()
             (setf *print-p* t))
            (:mousebuttonup
             ()
             (setf *print-p* nil))
            (:mousemotion
             (:x x :y y :state state)
             (when (= state 1)
               (setf *mouse-x* x
                     *mouse-y* y)))
            (:mousewheel
             (:y scroll)
             (cond
               (*ctrl-key* (setf *ngon* (max 3 (+ *ngon* scroll))))
               (*shift-key* (setf *ngon-rot* (+ *ngon-rot* (* 45 scroll))))
               (t (setf *ngon-rad* (+ *ngon-rad* scroll))))
             )
            (:idle
             ()
             (gfx-clear renderer)

             (draw-station renderer station)

             ;; --- testing ---
             (draw-test-object renderer)
                          
             (sdl2:render-present renderer)
             (sdl2:delay 30)
             
             (unless *run-program*
               (sdl2:push-event :quit)))
            (:quit () (setf *run-program* nil) t)))))))

(defun cmd-loop-thread ()
  (format t "#cmd> ")
  (finish-output)
  (let ((input (read-line)))
    (setf *run-program* (not (equal input "quit"))))
  
  (when *run-program*
    (cmd-loop-thread)))

(defvar *gfx-thread* nil
  "the current graphics thread, for debugging purposes")

(defun main ()
  (setf *run-program* t)
  (let ((gfx-thread (or *gfx-thread*
                        (sb-thread:make-thread #'gfx-loop-thread))))

    (setf *gfx-thread* gfx-thread)
    (cmd-loop-thread)
    
    ;; in case cmd-loop-thread closed without signalling to terminate the program
    (setf *run-program* nil)
    (sb-thread:join-thread gfx-thread)
    (setf *gfx-thread* nil)))

(defun start-bg-gfx ()
  "start a new graphic thread, unless one is already running"
  (unless (and *gfx-thread* (sb-thread:thread-alive-p *gfx-thread*))
    (setf *run-program* t
          *gfx-thread* (sb-thread:make-thread #'gfx-loop-thread :name "Graphics Test"))))

(defun join-bg-gfx ()
  (setf *run-program* nil)
  (sb-thread:join-thread *gfx-thread*))

(defun cleanup-broken-gfx-thread ()
  (sb-thread:terminate-thread *gfx-thread*)
  (setf *run-program* nil))
  
