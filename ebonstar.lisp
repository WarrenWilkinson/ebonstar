;(load "quicklisp.lisp")
;(quicklisp-quickstart:install)

(load "~/quicklisp/setup.lisp")
(ql:quickload "lispbuilder-sdl")
;(ql:quickload "lispbuilder-sdl-gfx")

(defvar *w* 1024)
(defvar *h* 768)

(defstruct (vect (:type (vector float)) (:constructor nil)) 
  (x 0.0 :type float)
  (y 0.0 :type float))

(defun vect (x y)
  (make-array 2
    :element-type 'float
    :initial-contents (list (coerce x 'float) (coerce y 'float))))

;(coerce 4 'float)

(vect 4 5)

(defun vect-magnitude (v)
  (sqrt (+ (expt (vect-x v) 2) (expt (vect-y v) 2))))

(defun vect-angle (v) (atan (vect-y v) (vect-x v)))

(defun vect-multiply (v scalar)
  (vect (* scalar (vect-x v)) (* scalar (vect-y v))))
  
(defun compute-gravity (xdist ydist gforce fallout)
  (let* ((v (vect xdist ydist))
	 (distance (vect-magnitude v))
	 (strength (if (< distance 10) 100000 (/ gforce (expt distance fallout)))))
    (vect-multiply v (* strength (abs distance)))))

;(defstruct vortex-template
;  (size nil)
;  (data nil))
;
;(defmacro defvortex (name size force fallout)
;  `(defconstant ,(intern (concatenate 'string "+" (symbol-name name) "-VORTEX+"))
;     (make-vortex-template 
;      :size ,size
;      :data 
;
;(defvortex medium 200 5000 1.8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *background* (sdl:color :r 10 :g 10 :b 10))
(defvar *grid* (sdl:color :r 10 :g 10 :b 230 :a 255))
(defvar *clear* (sdl:color :r 255 :g 255 :b 0 :a 0))
(defvar *purple* (sdl:color :r 255 :g 10 :b 230))
(defvar *green* (sdl:color :r 40 :g 255 :b 40))
(defvar *pink* (sdl:color :r 255 :g 155 :b 155))

;; Vortex size shouldn't be evenly divisibly by grid size... it should be 1 bigger to prevent a tearing effect.
(defstruct (vortex (:constructor mk-vortex (size &optional c &aux (color (or c *purple*)))))
  (size)
  (data)
  (xblits)
  (yblits)
  (color *purple*)
  (x 0)
  (y 0)
  (xv .25)
  (yv .25))

(defun vortex-left-position (v) (round (- (vortex-x v) (truncate (vortex-size v) 2))))
(defun vortex-top-position (v) (round (- (vortex-y v) (truncate (vortex-size v) 2))))

;(defvar *zero-vect* (vect 0 0))
;(defun vortex-gvector (v x y)
;  (let ((width (truncate (vortex-size v) 2)))
;    (if (or (< x (- 0 width))
;	    (> x (- width 1))
;	    (< y (- 0 width))
;	    (> y (- width 1)))
;	*zero-vect*
;	(aref (vortex-data v) (+ width x) (+ width y)))))

(defun compute-vortex-data (size force fallout)
  ;; TODO -- make it compute force and fallout appropriate for size.
  (make-array (list size size)
	      :element-type 'vector
	      :initial-contents
	      (loop for x from (- 0 (truncate size 2)) upto (1- (truncate size 2))
		 collecting (loop for y from (- 0 (truncate size 2)) upto (1- (truncate size 2))
			       collecting (compute-gravity x y force fallout)))))

(defun blend-color (ratio a b)
  (setf ratio (max 0 (min 1 ratio)))
  (multiple-value-bind (ar ag ab) (sdl:color-* a)
    (multiple-value-bind (br bg bb) (sdl:color-* b)
      (sdl:color :r (truncate (+ (* ar ratio)
				 (* br (- 1 ratio))))
	 :g (truncate (+ (* ag ratio)
			 (* bg  (- 1 ratio))))
	 :b (truncate (+ (* ab ratio)
			 (* bb  (- 1 ratio))))))))
  
(defun compute-vortex-pixel-v (x y offset grav surface color v-size) 
  (let ((dx (+ x (vect-x grav)))
	(dy (+ y (vect-y grav))))
    (if (> (vect-magnitude grav) 10000)
	*background*
	(if (zerop (mod (round dx) 20))
	    (let ((color (blend-color (/ (vect-magnitude grav) 3) color *grid*)))
	      (sdl:draw-pixel-* (mod (- x offset) v-size) y :surface surface :color color))
	    (sdl:draw-pixel-* (mod (- x offset) v-size) y :surface surface :color *clear*)))))

(defun compute-vortex-pixel-h (x y offset grav surface color v-size) 
  (let ((dx (+ x (vect-x grav)))
	(dy (+ y (vect-y grav))))
    (if (> (vect-magnitude grav) 10000)
	*background*
	(if (zerop (mod (round dy) 20))
	    (let ((color (blend-color (/ (vect-magnitude grav) 3) color *grid*)))
	      (sdl:draw-pixel-* x (mod (- y offset) v-size) :surface surface :color color))
	    (sdl:draw-pixel-* x (mod (- y offset) v-size) :surface surface :color *clear*)))))

(defun draw-vortex-vlines (v offset surface color v-size)
  (loop for x from 0 upto (1- (vortex-size v))
        do (loop for y from 0 upto (1- (vortex-size v))
	         do (compute-vortex-pixel-v x y offset (aref (vortex-data v) (mod (- x offset) v-size) y) surface color v-size))))

(defun draw-vortex-hlines (v offset surface color v-size)
  (loop for x from 0 upto (1- (vortex-size v))
        do (loop for y from 0 upto (1- (vortex-size v))
	         do (compute-vortex-pixel-h x y offset (aref (vortex-data v) x (mod (- y offset) v-size)) surface color v-size))))

(defun compute-vortex-blits (v)
  (let ((vlines (loop for offset from 0 upto 19
		      collecting (sdl:convert-surface 
				  :surface (let ((surface (sdl:create-surface (vortex-size v) (vortex-size v) :pixel-alpha t :type :sw)))
					     (draw-vortex-vlines v offset surface (vortex-color v) (vortex-size v))
					     surface)
				  :free t
				  :type :hw)))
	(hlines (loop for offset from 0 upto 19
		      collecting (sdl:convert-surface 
				  :surface (let ((surface (sdl:create-surface (vortex-size v) (vortex-size v) :pixel-alpha t :type :sw)))
					     (draw-vortex-hlines v offset surface (vortex-color v) (vortex-size v))
					     surface)
				  :free t
				  :type :hw))))
    
    (setf (vortex-xblits v) (coerce vlines '(array sdl:surface (20))))
    (setf (vortex-yblits v) (coerce hlines '(array sdl:surface (20))))
    v))

(defun make-vortex (size force fallout &optional color)
  (let ((v (mk-vortex size color)))
    (setf (vortex-data v) (compute-vortex-data size force fallout))
    (compute-vortex-blits v)
    v))

(defvar *vortexes* nil)

(defun draw-box (x1 y1 w h &key color)
  (loop for y from y1 upto (1- (+ y1 h))
        do (sdl:draw-hline x1 (1- (+ x1 w)) y :color color)))

(defun draw-grid-lines (vortexes)
  (declare (ignore vortexes))
  ;; TODO don't draw where the vortexes will be.
  (loop for x from 0 to *w* by 20
        do (sdl:draw-line-* x 0 x *h* :color *grid*))
  (loop for y from 0 to *h* by 20
        do (sdl:draw-hline 0 *w* y :color *grid*)))

;(DECLAIM (inline compute-vortex-pixel))
;;; Is there a way to do the other way with this? Instead of the grid, just trace the lines? 
;(defun compute-vortex-pixel (x y grav) 
;  (let ((dx (+ x (vect-x grav)))
;	(dy (+ y (vect-y grav))))
;    (if (or (zerop (mod (round dx) 20))
;	    (zerop (mod (round dy) 20)))
;	(sdl:draw-pixel-* x y :color *grid*))))

(defun draw-vortex (v)
  (draw-box (vortex-left-position v) (vortex-top-position v) (vortex-size v) (vortex-size v) :color *background*)
  (let ((xblit (elt (vortex-xblits v) (mod (vortex-left-position v) 20)))
	(yblit (elt (vortex-yblits v) (mod (vortex-top-position v) 20))))
    (sdl:set-position-* xblit :x (vortex-left-position v) :y (vortex-top-position v))
    (sdl:blit-surface xblit)
    (sdl:set-position-* yblit :x (vortex-left-position v) :y (vortex-top-position v))
    (sdl:blit-surface yblit)))

;  (loop for x from 0 upto (1- (vortex-size v))
;        with xo = (vortex-left-position v)
;        do (loop for y from 0 upto (1- (vortex-size v))
;		 with yo = (vortex-top-position v)
;		 do (compute-vortex-pixel xo yo (aref (vortex-data v) x y))
;		 do (incf yo))
;        do (incf xo)))
          
(defun draw-vortexes (vortexes)
  ;; should do bounding boxes if vortexes are going to collide.
  (dolist (v vortexes)
    (draw-vortex v)))

(defun draw-sprites ()
  nil)

(defun draw-scene ()
  (sdl:clear-display *background*)
  (draw-grid-lines *vortexes*)
  (draw-vortexes *vortexes*)
  (draw-sprites))

(defun run-vortex (vortex)
  (incf (vortex-x vortex) (vortex-xv vortex))
  (incf (vortex-y vortex) (vortex-yv vortex))
  
  (when (> (vortex-x vortex) *w*)
    (setf (vortex-xv vortex) (- 0 (abs (vortex-xv vortex)))))
  (when (< (vortex-x vortex) 0)
    (setf (vortex-xv vortex) (abs (vortex-xv vortex))))

  (when (> (vortex-y vortex) *h*)
    (setf (vortex-yv vortex) (- 0 (abs (vortex-yv vortex)))))
  (when (< (vortex-y vortex) 0)
    (setf (vortex-yv vortex) (abs (vortex-yv vortex)))))

(defun run-ai ()
  (dolist (v *vortexes*)
    (run-vortex v)))

(defun generate-vortexes ()
  (setf *vortexes* (list (make-vortex 200 130 3.2 *purple*) (make-vortex 260 230 3.2 *green*) (make-vortex 300 300 3.2 *pink*)))
  (setf (vortex-x (second *vortexes*)) 100)
  (setf (vortex-xv (second *vortexes*)) .05)
  (setf (vortex-y (second *vortexes*)) *h*)
  (setf (vortex-yv (second *vortexes*)) .05)
  
  (setf (vortex-x (third *vortexes*)) *w*)
  (setf (vortex-yv (third *vortexes*)) .1)
  (setf (vortex-xv (third *vortexes*)) .1))
  
(defvar *fcounter* 0)
(defun run ()
  (sdl:with-init ()
    (sdl:window *w* *h* :SW t :BPP 32)
					;(sdl:draw-surface (sdl-image:load-image "code.bmp"))
					;(sdl:clear-display *background*)
					;(draw-grid)
    (sdl:update-display)
    (setf (sdl:frame-rate) 0)
    (generate-vortexes)

    (sdl:with-events (:POLL)
      (:quit-event () t)
      (:KEY-DOWN-EVENT (:KEY KEY) 
		       (WHEN (SDL:KEY= KEY :SDL-KEY-ESCAPE) 
			 (SDL:PUSH-QUIT-EVENT)))
      (:video-expose-event () (sdl:update-display))
      (:idle 
       (when (mod (incf *fcounter*) 1000) (format t "~%FPS: ~a" (coerce (sdl:average-fps) 'float)))
       (run-ai)
       (draw-scene)
       (sdl:update-display)))))

(require :sb-sprof)

(defun make-vortexes ()
  (sdl:with-init ()
    (sdl:window *w* *h* :SW t :BPP 32)
    (generate-vortexes)))

(sb-sprof:with-profiling (:max-samples 1000
				       :report :flat
				       :loop nil)
  (make-vortexes))
  
;  (run))

;; On my pretty powerful machine, computing the vortexes destroys it.  I'm 2.6 ghz, and 3 vortexes is bringing it down to 40 fps. Otherwise it's at a cool 220fps.
;; How can I make this faster... one way would be to know that I'm only rendering horizontal lines... and those are independent in the space... 
;; Basically...  I think I can produce 'rows of offset' tables...  Then its almost like drawing grid lines... Instead I just say this is my set of V lines, this is my
;; set of H lines. Draw them! 

;; This method is fast enough, but kind of inflexible. Drawing all the vortexs is really slow at startup
;; (one vortext = WxHxGxG pixel plot ops.

;; HMMMM... but I could precompute those even. Basically, plot it with alpha.  To color the vortex, copy that data onto a sheet of color and save the result as 
;; a frame.  Unfortunately, because I don't generate them at run time, I can't play with the grid at all -- the grid is decided at compile time.

;; SO: predefined vortexes.  This stuff about floating point really sucks.  It means I honestly do have to switch to fixed point.  It's not such a big
;; deal for me, but there you go. And I need to start integrating E-NET and dead reckoning sort of things.  The vortex is dead reckoned.  He just goes
;; along a set patch (and can't really be altered). Ships kind of same but they change often.  Bullets are dead-reckoned, till they hit vortex...
;; but that can be okay. Grav is set (Dead Reckon Alg: Vortex-NSIZE.  Bullets are Dead Reckon: Bullet.  Fireballs are Dead Reckon and ships.... that's about
;; all).

;; So architecture then... I have a few objects (spaceships) controllable by people. Basically they can do the same thing.  Have a mask, blit it onto a solid
;; color swatch, for all degrees of rotation, with trusters on/off.   Vortexs are the same -- blit all sprites onto a gradient background. Ships onto solid
;; (user defined background). 
;;
;; Then, I should seperate the ships (ai) and vortex from the sprite thing.  Reason being so I can test them seperately.   So I have my 'objects' in universe
;; simulation, and the rendering is entirely separate.  Grid color can be changed (because the radial gradient can be applied to the background color). 
;; So then I just start defining my objects, using a fixed point math.
;;
;; Debuggability? 
;;
;; I think replayability is an okay goal for this system. That means time is discrete events, not necessarily driven by clock. It also means graphics
;; and sound are TOTALLY independent (representation). All my system is a simulator of shared world. Graphics renders elsewhere.  It also means
;; my communication protocol should be MOSTLY text. I can spare the cycles to parse it. And the future proofing is good for me, in that my 'tests'
;; can be built upon a stack of crashes. By default I should have logging on, and use it to save replays. And I can use it for the intro videos.
;;
;; What's the quickest thing I can do on this? A simple chat server is it, using E_NET.  Start by letting it send text messages. I should also support
;; a 'DEVEL' text message... its a fake packet I can manually insert, and use to say things like 'watch this here...' 

(run)