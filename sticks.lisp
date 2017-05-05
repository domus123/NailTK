(ql:quickload "ltk")
(defpackage :nailtk
  (:use :ltk :cl :common-lisp :cl-user)
  (:export :stick))



(in-package :nailtk)
;;(in-package :ltk)

(defparameter xx 0)
(defparameter yy 0)
(defparameter scolor 'red)
(defparameter *xsize* 600)
(defparameter *ysize* 400)
(defparameter *items* '())

(defun rotatelist()
  (when (consp *items*)
    (let ( (xx (pop *items*)))
      (setf *items* (append *items* (list xx)) )) ))

(defun snp (x)
  (* (floor (/ x 10)) 10))

(defun stick ()
  (with-ltk ()
	    (let* (  (f (make-instance 'frame))
                     (g (make-instance 'frame))	  
		     (bt1 (make-instance 'button :text "Blue"
				       :master f
				       :command (lambda ()
						  (setf scolor 'blue))))
		     (bt2 (make-instance 'button :text "Red"
				       :master f
				       :command (lambda ()
						  (setf scolor 'red))))
		     (bt3 (make-instance 'button :text "Gray"
				       :master f
				       :command (lambda ()
						  (setf scolor 'gray))))
		     (bt4 (make-instance 'button :text "Black"
				       :master f
				       :command (lambda ()
						  (setf scolor 'black))))
		     (bt5 (make-instance 'button :text "Yellow"
				       :master f
				       :command (lambda ()
						  (setf scolor 'yellow))))
		     (canvas (make-instance 'canvas
					    :width *xsize*
					    :height *ysize*))
		     (bt6 (make-instance 'button :text "Rotate"
				:master f
			        :command (lambda ()
					   (when (consp *items*)
					     (itemconfigure canvas
						  (car *items*)
						  :outline :white)
					     (rotatelist)
					     (itemconfigure canvas
							    (car *items*)
						     :outline :black))) ))
		     (ext (make-instance 'button :text "Exit"
					 :master f
					 :command
					 (lambda ()
					   (setf *exit-mainloop* t)) ))
		     (mause (make-instance 'entry
					   :text "00, 00"
					   :width 8
					   :master f))
		     (pt1 (make-instance 'button :text "Up"
				       :master g
				       :command
			     (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (- (caddr rxy) 1))
					(xx1 (cadddr rxy))
					(yy1 (- (cadddr (cdr rxy)) 1) ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					   (list xx yy xx1 yy1 )) ))))
		     (pt2 (make-instance 'button :text "Down"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (+ (caddr rxy) 1))
					(xx1 (cadddr rxy))
					(yy1 (+ (cadddr (cdr rxy)) 1) ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					   (list xx yy xx1 yy1 )) ))))
		     (pt3 (make-instance 'button :text "Left"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (- (cadr rxy) 1))
					(yy (caddr rxy))
					(xx1 (- (cadddr rxy) 1))
					(yy1 (cadddr (cdr rxy)) ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					    (list xx yy xx1 yy1 )) ))))
                     (pt4 (make-instance 'button :text "Right"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (+ (cadr rxy) 1))
					(yy (caddr rxy))
					(xx1 (+ (cadddr rxy) 1))
					(yy1 (cadddr (cdr rxy))  ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					    (list xx yy xx1 yy1 )) ))))
		     (pt5 (make-instance 'button :text "+width"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (caddr rxy))
					(xx1 (+ (cadddr rxy) 1))
					(yy1 (cadddr (cdr rxy))  ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					   (list xx yy xx1 yy1 )) ))))
		      (pt6 (make-instance 'button :text "-width"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (caddr rxy))
					(xx1 (- (cadddr rxy) 1))
					(yy1 (cadddr (cdr rxy))  ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					    (list xx yy xx1 yy1 )) ))))
		      (pt7 (make-instance 'button :text "+height"
				       :master g
				       :command
				       (lambda ()
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (caddr rxy))
					(xx1 (cadddr rxy))
					(yy1 (+ (cadddr (cdr rxy)) 1)  ))
				  (push (list rect xx yy xx1 yy1)
					*items*)
					(set-coords canvas rect
					    (list xx yy xx1 yy1 )) ))))
		      (pt8 (make-instance 'button :text "-height"
				       :master g
				       :command
				       (lambda ()
			    (let* ( (rxy (pop *items*))
				    (rect (car rxy))
				    (xx (cadr rxy))
				    (yy (caddr rxy))
				    (xx1 (cadddr rxy))
				    (yy1 (- (cadddr (cdr rxy)) 1)  ))
			      (push (list rect xx yy xx1 yy1) *items*)
				 (set-coords canvas rect
				     (list xx yy xx1 yy1))) )))
                    (down nil))
	      (pack f :anchor :nw)
	      (pack bt1 :side :left)
	      (pack bt2 :side :left)
	      (pack bt3 :side :left)
	      (pack bt4 :side :left)
	      (pack bt5 :side :left)
	      (pack bt6 :side :left)
	      (pack ext :side :left)
	      (pack mause)
	      (pack pt1 :side :left)
	      (pack pt2 :side :left)
	      (pack pt3 :side :left)
	      (pack pt4 :side :left)
	      (pack pt5 :side :left)
	      (pack  pt6 :side :left)
	      (pack pt7 :side :left)
	      (pack pt8)
	      (configure f :borderwidth 3)
	      (configure f :relief :sunken)
              (configure g :borderwidth 3)
	      (configure g :relief :sunken)
     (pack canvas)
     (pack g :after canvas)
     (pack f :after g)
    
     (bind canvas "<ButtonPress-1>"
           (lambda (evt)
             (setf down t)
	     (when t
	       (setf xx (snp (event-x evt))
	    	     yy (snp (event-y evt))) )))
     (bind canvas "<ButtonRelease-1>"
           (lambda (evt) (setf down t)
	     (when (> xx 0) 
               (let* ( (x0 (snp (event-x evt))) (y0 (snp (event-y evt)))
		       (xx1 0) (yy1 0)
		       (rect (cond ( (equal scolor 'black)
				    (setf xx1 (+ xx 10) yy1 (+ yy 10))
				    (create-rectangle canvas
					 xx yy xx1 yy1))
			           ( (> (- x0 xx) (- y0 yy))
				    (setf xx1 (+ (snp (event-x evt)) 10)
					  yy1 (+ yy 10))
				     (create-rectangle canvas xx yy 
				       xx1 yy1))
			           (t (setf xx1 (+ xx 10)
					    yy1 (+ (snp (event-y evt)) 10))
				      (create-rectangle canvas xx yy
					 xx1 yy1 )) )))
		 (cond ( (equal scolor 'red)
			 (itemconfigure canvas rect :fill :red)
			 (itemconfigure canvas rect :outline :red))
		       ( (equal scolor 'blue)
			 (itemconfigure canvas rect :fill :blue)
			 (itemconfigure canvas rect :outline :blue))
		       ( (equal scolor 'yellow)
			 (itemconfigure canvas rect :fill :yellow)
			 (itemconfigure canvas rect :outline :yellow))
		       ( (equal scolor 'black)
			 (itemconfigure canvas rect :fill :black)
			 (itemconfigure canvas rect :outline :black))
		       (t  (itemconfigure canvas rect :fill :gray)
			   (itemconfigure canvas rect :outline :gray)))
                 (push (list rect xx yy xx1 yy1) *items*)
		 (setf xx 0 yy 0)   )) ))
     (bind canvas "<ButtonRelease-2>"
	   (lambda(evt)
	     (declare (ignore evt))
	     (itemconfigure canvas (car (pop *items*)) :state :hidden)))
     (bind canvas "<Motion>"
	   (lambda(evt)
	     (setf (ltk:text mause)
		 (format nil "~a, ~a" (event-x evt)
			 (event-y evt)))  )) )))


