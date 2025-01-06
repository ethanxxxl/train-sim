(require :sdl2)
(require :sdl2-gfx)

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

(defstruct test-object
  surface t)

(defun draw-test-object (renderer)
  ;(sdl2-gfx:rounded-rectangle-color )
  ;(sdl2-gfx:rotozoom-surface)
  (sdl2-gfx::aacircle-rgba renderer 250 100 50 100 230 100 255))

(defvar *run-program* nil
  "set to true during main function, when threads are created and graphics is
started.")

(defvar *renderer* nil "variable for debugging purposes")
(defvar +screen-width+ 640)
(defvar +screen-height+ 480)
(defun gfx-loop-thread ()
"use SDL to create a graphics window and draw stuff"
  (let ((station (make-station :name "test-station" :x 50 :y 50)))
    ;; initialization
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "SDL2 from Common Lisp" :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (setf *renderer* renderer)
          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (gfx-clear renderer)

             (draw-station renderer station)

             ;; --- testing ---
             (draw-test-object renderer)
                          
             (sdl2:render-present renderer)
             (sdl2:delay 33)
             
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
  (setf *run-program* t
        *gfx-thread* (sb-thread:make-thread #'gfx-loop-thread)))

(defun join-bg-gfx ()
  (setf *run-program* nil)
  (sb-thread:join-thread *gfx-thread*))
