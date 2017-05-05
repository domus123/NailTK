(ql:quickload "ltk")
(defpackage :nailtk
  (:use :ltk :cl :common-lisp :cl-user)
  (:export :stick
	   :save-frame))
(in-package :nailtk)

(defparameter xx 0)
(defparameter yy 0)
(defparameter scolor 'red)
(defparameter *xsize* 600)
(defparameter *ysize* 400)
(defparameter *items* '())
(defparameter *output-file* nil)
(defparameter *version* 0.01)

(defun rotatelist()
  (when (consp *items*)
    (let ( (xx (pop *items*)))
      (nconc *items* (cons xx nil)) )))

(defun snp (x)
  (* (floor (/ x 10)) 10))

(defun change-value (canvas xh yh xs ys)
  (let* ( (rxy (pop *items*))
	 (rect (car rxy))
	  (xx (+ (cadr rxy) xs))
	  (yy (+ (caddr rxy) ys))
	  (xx1 (+ (cadddr rxy) xh))
	  (yy1 (+ (cadddr (cdr rxy)) yh)))
    (push (list rect xx yy xx1 yy1) *items*)
    (set-coords canvas rect
		(list xx yy xx1 yy1 )) ))

(defun save-frame ()
  (with-ltk ()
    (let* ( (sf (make-instance 'frame))
	   (input-name (make-instance 'text
				       :width 20
				       :height 1
				       :master sf))
	    (save-button (make-instance 'button
				      :text "Save"
				      :master sf
				      :command
				      (lambda () (let ( (output (text input-name)))
						   (setf *output-file* output
							 *exit-mainloop* t))) ))
	    (f-name (make-instance 'label
				  :text "File Name:"
				  :master sf))
	   (scanvas (make-instance 'canvas
				   :width 200
				   :height 10)))
	   
      (pack scanvas)
      (pack sf :anchor :nw)
      (pack f-name :side :left)
      (pack input-name :side :left)
      (pack save-button :side :right))))

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
					   (setf *exit-mainloop* t)
					   (format t "Thank you!~%")) ))
		     (mause (make-instance 'entry
					   :text "00, 00"
					   :width 8
					   :master f))
		     (dlt (make-instance 'button :text "Delete"
					 :master f
					 :command (lambda () (when (consp *items*)
							  (let* ( (rem (pop *items*))
								 (elem (car rem))
								  (posix '(0 0 0 0)))
							    (set-coords canvas elem posix)))) ))

		     (save (make-instance 'button :text "Save"
					  :master f
					  :command (lambda () (nailtk::save-frame)
							   (if (null *output-file*) (format t "File name missing")
							       (when (postscript canvas *output-file*)
								 (format t "~a CREATED" *output-file* ))) )))
		     (pt1 (make-instance 'button :text "Up"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 -1 0 -1)) ))
		     (pt2 (make-instance 'button :text "Down"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 1 0 1)) ))
		     (pt3 (make-instance 'button :text "Left"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas -1 0 -1 0)) ))
					
                     (pt4 (make-instance 'button :text "Right"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 1 0 1 0)) ))
		     (pt5 (make-instance 'button :text "+width"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 0 -1 0)) ))
		     (pt6 (make-instance 'button :text "-width"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 0 +1 0)) ))
		      (pt7 (make-instance 'button :text "+height"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 0 0 -1)) ))
		     (pt8 (make-instance 'button :text "-height"
				       :master g
				       :command
				       (lambda ()
					 (change-value canvas 0 0 0 +1)) ))
		     (down nil))
	      (pack f :anchor :nw)
	      (pack bt1 :side :left)
	      (pack bt2 :side :left)
	      (pack bt3 :side :left)
	      (pack bt4 :side :left)
	      (pack bt5 :side :left)
	      (pack bt6 :side :left)
	      (pack dlt :side :left)
	      (pack save :side :left)
	      (pack ext :side :left)
	      (pack mause)
	      (pack pt1 :side :left)
	      (pack pt2 :side :left)
	      (pack pt3 :side :left)
	      (pack pt4 :side :left)
	      (pack pt5 :side :left)
	      (pack pt6 :side :left)
	      (pack pt7 :side :left)
	      (pack pt8)
	      (pack f :anchor :nw)	      
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
		 (format nil "~ax,~ay" (event-x evt)
			 (event-y evt)))  )) )))
(let ()
  (format t "~&-----------------NailTK-------------------")
  (format t "~&Natural and artificial laboratory")
  (format t "~&Federal University of Uberlândia")
  (format t "~&Vers : ~a" *version*)
  (format t "~&May/2017")
  (format t "~&------------------------------------------~%")
  (finish-output t)
  (nailtk::stick))
