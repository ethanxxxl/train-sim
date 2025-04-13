(require :sdl2)
(load "./sdl-vector.lisp")
(load "./gfx-utils.lisp")

(defstruct station
           "describes a train station that trains may connect to."
  name
  x y)

(defun draw-station (renderer station)
  "Draws the station onto the screen"
  )

(defun draw-train ())

(defun draw-car (renderer car))
(defun draw-engine (renderer engine))

(defun draw-track ())

(defvar *point-list* nil)
(defvar *mouse-x* 1)
(defvar *mouse-y* 1)
(defvar *ngon* 32)
(defvar *ngon-rad* 100)
(defvar *ngon-rot* 0)
(defun draw-test-object (renderer)
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
      (sdl2-ffi.functions:sdl-gl-set-attribute
       sdl2-ffi:+sdl-gl-multisamplebuffers+ 1)
      (sdl2:with-window (win :title "SDL2 from Common Lisp" :flags '(:shown))      
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (setf *renderer* renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (alexandria:switch ((sdl2:scancode-value keysym)
                                 :test 'sdl2:scancode=)
               (:scancode-lctrl (setf *ctrl-key* t))
               (:scancode-lshift (setf *shift-key* t))
               (:scancode-r (when *ctrl-key* (pop *point-list*)))))
            (:keyup
             (:keysym keysym)
             (alexandria:switch ((sdl2:scancode-value keysym)
                                 :test 'sdl2:scancode=)
               (:scancode-escape (sdl2:push-event :quit))
               (:scancode-lctrl (setf *ctrl-key* nil))
               (:scancode-lshift (setf *shift-key* nil))
               (:scancode-return (push (cons *mouse-x* *mouse-y*) *point-list*))))
            (:mousebuttondown
             (:x x :y y)
             (when *ctrl-key*
               (push (cons x y) *point-list*))
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
             (sdl2:delay 10)
             
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
  
