;;; Author: Ethan Smith

;;; This is an extension to the SDL2 package, and provides support for the
;;; SDL_RenderGeometry function, along with facilities for generating vertices
;;; and indices.

(in-package :sdl2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertices

(defun make-vertices (num)
  "allocates a c-array of sdl-vertex with length num.  This needs to be free'd.
Element access through c-ref is recommended."
  (c-let ((verts sdl2-ffi:sdl-vertex :count num)) verts))

(defun make-vertices (positions &optional colors tex-coords)
  "allocates a c-array of sdl-vertex.  The number of vectors produced is determined
by longest number of entries."
  (if (and (= 0 (mod (length positions) 2))
           (= 0 (mod (length colors) 4))
           (= 0 (mod (length tex-coords) 2))) 

      ;; `colors' has twice as many elements per entry than either `positions' or
      ;; `tex-coords'
      (let ((len (max (/ (length colors) 4)
                      (/ (length positions) 2)
                      (/ (length tex-coords) 2)))

            ;; vertex default values
            (x 0.0) (y 0.0)
            (r 255) (g 255) (b 255) (a 255)
            (u 0.0) (v 0.0))

        (c-let ((verts sdl2-ffi:sdl-vertex :count len))
          (loop
            with pos = positions
            with color = colors
            with tex = tex-coords

            for i from 0 below len

            do (progn
                 (setf x (float (or (nth 0 pos) x))
                       y (float (or (nth 1 pos) y))

                       u (float (or (nth 0 tex) u))
                       v (float (or (nth 1 tex) v))
                       
                       r (or (nth 0 color) r)
                       g (or (nth 1 color) g)
                       b (or (nth 2 color) b)
                       a (or (nth 3 color) a))

                 (setf (verts i :position :x) x
                       (verts i :position :y) y
                       
                       (verts i :tex-coord :x) u
                       (verts i :tex-coord :y) v

                       (verts i :color :r) r
                       (verts i :color :g) g
                       (verts i :color :b) b
                       (verts i :color :a) a)

                 (setf pos (nthcdr 2 pos)
                       color (nthcdr 4 colors)
                       tex (nthcdr 2 tex-coords))))

          (values verts len)))
      (error "positions, colors, and tex-coords must be multiples of 2,4, or 2")))

(defun free-vertices (verts)
  (cffi:foreign-free (ptr verts))
  (autowrap:invalidate verts))

(defmacro %with-verts ((vertex-ptr num-verts positions &optional colors tex-coords)
                       &body body)
  `(unwind-protect  
        (multiple-value-bind (,vertex-ptr ,num-verts)
            (make-vertices ,positions ,colors ,tex-coords)
          (unwind-protect (progn ,@body)
            (free-vertices ,vertex-ptr)))))

(defmacro with-verts (bindings &body body)
  "bindings is a list of the forms in the following format: (vert-ptr-var len-var
positions &optional colors tex-coords).  The `positions' `colors' and
`tex-coords' variables will be passed to `make-vertices' and its results bound
to `vert-ptr-var' and `len-var'.  Like a LET binding, `positions' `colors' and
`tex-coords' are evaluated.  All allocated resources are free'd after `body' is
evaluated.'"
  (if bindings
      `(%with-verts ,(car bindings)
         (with-verts ,(cdr bindings) ,@body))
      `(progn ,@body)))

(defun print-vertices (vertex-ptr num-verts)
  (c-with ((verts sdl2-ffi:sdl-vertex :from vertex-ptr))
    (loop for n from 0 below num-verts
          do (format t "~S] x,y: ~S,~S | rgba: ~S,~S,~S,~S | u,v: ~S,~S~%"
                     n
                     (verts n :position :x)
                     (verts n :position :y)
                     (verts n :color :r)
                     (verts n :color :g)
                     (verts n :color :b)
                     (verts n :color :a)
                     (verts n :tex-coord :x)
                     (verts n :tex-coord :y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indices 
(defun make-index-ptr (&rest indices)
  (let ((len (length indices)))
    (c-let ((index-ptr :int :count len))
      (loop for idx in indices
            for i from 0
            do (setf (index-ptr i) idx))
      (index-ptr &))))

(defun free-index-ptr (index-ptr)
  (cffi:foreign-free index-ptr))

(defmacro %with-indices ((idx-var len-var bind-val) &body body)
  `(let ((,idx-var (apply 'make-index-ptr ,bind-val))
         (,len-var (length ,bind-val)))
     (unwind-protect (progn ,@body)
       (free-index-ptr ,idx-var))))

(defmacro with-indices (bindings &body body)
  "`bindings' is in the form ((ptr-var1 len-var1 indices) (ptr2-val2 len-var2
indices) ...), and each variable is bound to an integer array containing the
indices, and `body' is executed in this scope.  Just as in a LET form, the
bind-value is evaluated.  All allocated resources are free'd after `body' is
executed."
  (if bindings
      `(%with-indices ,(car bindings)
         (with-indices ,(cdr bindings) ,@body))
      `(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry

(defun render-geometry (renderer
                        texture
                        vertices num-vertices
                        indices num-indices)
  (check-rc (sdl-render-geometry renderer
                                 texture
                                 vertices num-vertices
                                 indices num-indices)))

(export (list 'render-geometry 'with-verts 'with-indices 'print-vertices) :sdl2)
