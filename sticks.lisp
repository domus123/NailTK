(ql:quickload "ltk")
(defpackage :nailtk
  (:use :ltk :cl :common-lisp :cl-user)
  (:export :stick
	   :save-frame
	   :save-ntk-file))
(in-package :nailtk)

(defparameter xx 0)
(defparameter yy 0)
(defparameter scolor 'red)
(defparameter *xsize* 900)
(defparameter *ysize* 800)
(defparameter *items* '())
(defparameter *output-file* "default")
(defparameter *version* 0.01)
(defparameter *ntk* 0) ;;flag for saving file as .ntk
(defparameter *img* 0) ;;flag for saving file as .png

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

(defun save-ntk-file ()
  (with-open-file (stream (format nil "~a.ntk" *output-file*)
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "~a" *items*)))

(defun save-frame ()
  (with-ltk ()
    (let* ( (sf (make-instance 'frame ))
	    (check (make-instance 'frame))
	    (input-name (make-instance 'text
				       :width 20
				      :height 1
				      :master sf))
	    (save-button (make-instance 'button
				      :text "Save"
				      :master sf
				      :command
				      (lambda () (when (text input-name)
						   (let ( (output (text input-name)))
						     (setf *output-file* output
							   *exit-mainloop* t)))) ))
	    (ext (make-instance 'button
				:text "Close"
				:master sf
				:command (lambda () (setf *exit-mainloop* t))))
	    (f-name (make-instance 'label
				   :text "File Name:"
				  :master sf))
	    (save-as(make-instance 'label
					:text "Save as:"
					:master check))
	    (scanvas (make-instance 'canvas
				   :width 200
				   :height 10))
	    (check-ntk (make-instance 'check-button
				      :master check
				      :text "Ntk File"
				      :command (lambda (e)
						 (setf *ntk* e))))
	    (check-img (make-instance 'check-button
				      :text "Png file"
				      :master check
				      :command (lambda (e)
						 (setf *img* e)) ))) 
      (pack scanvas)
      (pack check :anchor :nw)
      (pack sf :anchor :nw)
      (pack f-name :side :left)
      (pack input-name :side :left)
      (pack ext :side :bottom)
      (pack save-button :side :bottom)
      (pack save-as :side :left )
      (pack check-ntk :side :left)
      (pack check-img :side :left))))
      
(defun stick ()
  (with-ltk ()
	    (let* (  (f (make-instance 'frame))
                     (g (make-instance 'frame))	  
		     (menu (make-instance 'frame))
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
		     (load (make-instance 'button :text "Load"
					  :master menu
					  :command (lambda () )))
		     (ext (make-instance 'button :text "Exit"
					 :master menu
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
					  :master menu
					  :command (lambda () (nailtk::save-frame)
							   (cond ( (and (zerop *img*) (equal 1 *ntk*)) ;;check ntk| not check img
								  (nailtk::save-ntk-file))
								 ( (and (zerop *ntk*) (equal 1 *img*)) ;;check img| not check ntk
								  (postscript canvas *output-file*))
								 ( (and (equal 1 *ntk*) (equal 1 *img*)) ;; check both
								  (progn (nailtk::save-ntk-file)
									 (postscript canvas *output-file*)))
								 ( t (format t "~&None~%")))
							   (setf *ntk* 0
								 *img* 0))))
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
	      (pack g :anchor :nw)
	      (pack bt1 :side :left)
	      (pack bt2 :side :left)
	      (pack bt3 :side :left)
	      (pack bt4 :side :left)
	      (pack bt5 :side :left)
	      (pack bt6 :side :left)
	      (pack dlt :side :left)
	      (pack menu :anchor :nw)
	      (pack ext :side :left)
	      (pack save :side :left)
	      (pack load :side :left)
	      (pack mause)
	      (pack pt1 :side :left)
	      (pack pt2 :side :left)
	      (pack pt3 :side :left)
	      (pack pt4 :side :left)
	      (pack pt5 :side :left)
	      (pack pt6 :side :left)
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

