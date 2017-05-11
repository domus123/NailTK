(ql:quickload "ltk")
(defpackage :nailtk
  (:use :ltk :cl :common-lisp :cl-user)
  (:export :stick
	   :save-frame
	   :save-ntk-file
	   :main
	   :compile-ntk
	   :read-ntk-file
	   :config
	   :load-frame))

(in-package :nailtk)
(defparameter xx 0)
(defparameter yy 0)
(defparameter scolor 'red)
(defparameter *xsize* 600)
(defparameter *ysize* 600)
(defparameter *items* '())
(defparameter *output-file* "default")
(defparameter *version* 0.03)
(defparameter *ntk* 0) ;;flag for saving file as .ntk
(defparameter *img* 0) ;;flag for saving file as image

(defun config (xsize ysize)
  (when (and (not (null xsize)) (not (null ysize)))
    (with-open-file (stream ".config"
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "(~a . ~a)" xsize ysize))
    (format t "Xsize seted to -> ~a~%" xsize)
    (format t "Ysize seted to -> ~a~%" ysize)))

(defun read-config ()
  (with-open-file (stream ".config")
	(let ( (lst (read stream)))
	  (when (not (null lst))
	    (let ((xsize (car lst))
		  (ysize (cdr lst)))
	      (setf *xsize* xsize
		    *ysize* ysize)) ))))
  
(defun read-ntk-file (&optional (filename nil))
  (when (not (null filename))
    (with-open-file (stream filename)
      (loop for lines = (read stream nil :eof)
	 until (equal lines :eof)
	 collect lines)) ))

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
	  (yy1 (+ (cadddr (cdr rxy)) yh))
	  (color (car (last rxy)) ))
    (push (list rect xx yy xx1 yy1 color) *items*)
    (set-coords canvas rect
		(list xx yy xx1 yy1 )) ))

(defun save-ntk-file ()
  (let ( (fname (format nil "~a.ntk" (string *output-file*)) ))
    (with-open-file (stream fname
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "~a" *items*)) ))

(defun load-frame()
  (with-ltk ()
	    (let* ( (sf (make-instance 'frame))
		    (file-name (make-instance 'label
					      :text "File name: "
					      :master sf))
		    (input-name (make-instance 'text
					       :width 20
					       :height 1
					       :master sf))
		    (load-button (make-instance 'button
						:text "Load"
						:width 5
						:master sf
						:command (lambda () (format t "Pressed~%")) ))
		    (exit-button (make-instance 'button
						:text "Close"
						:width 5
						:master sf
						:command (lambda () (setf *exit-mainloop* t)) )))
				 
	      (pack sf :anchor :nw)
	      (pack file-name :side :left)
	      (pack input-name :side :left)
	      (pack load-button :side :left)
	      (pack exit-button :side :left)) ))

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
				      (lambda () 
					(let ( (output (string-trim '(#\space  #\newline #\tab) (text input-name)) ))
					    (setf *output-file* output
						  *exit-mainloop* t)))))
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
				      :text "Image file"
				      :master check
				      :command (lambda (e)
						 (setf *img* e)) ))) 
      (pack scanvas)
      (setf (text input-name) "dflt")
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
		     (bt1 (make-instance 'button :text "nDiff"
				       :master f
				       :command (lambda ()
						  (setf scolor 'green))))
		     (bt2 (make-instance 'button :text "Poly"
				       :master f
				       :command (lambda ()
						  (setf scolor 'red))))
		     (bt3 (make-instance 'button :text "Metal"
				       :master f
				       :command (lambda ()
						  (setf scolor 'gray))))
		     (bt4 (make-instance 'button :text "Connect"
				       :master f
				       :command (lambda ()
						  (setf scolor 'black))))
		     (bt5 (make-instance 'button :text "pDiff"
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
						  (caar *items*)
						  :outline :white)
					     (rotatelist)
					     (itemconfigure canvas
							    (caar *items*)
						     :outline :cyan))) ))
		     (load (make-instance 'button :text "Load" ;; Not working yet
					  :master menu
					  :command (lambda () 
						     (nailtk::load-frame)
						     (loop for elem in (car (read-ntk-file "dflt.ntk"))
								  do
							  (let* ( (item (car elem))
								 (posix (cdr elem))
								  (xx (car posix))
								  (yy (cadr posix))
								  (xx1 (caddr posix))
								  (yy1 (cadddr posix))
								  (color (string (car (last posix))))
								  (rect (create-rectangle canvas xx yy xx1 yy1)))
							    (push (cons item posix) *items*)
							    (cond ( (equal "RED" color)
								   (itemconfigure canvas rect :fill :red)
								    (itemconfigure canvas rect :outline :cyan))
								  ( (equal "GREEN" color)
								   (itemconfigure canvas rect :fill :green)
								    (itemconfigure canvas rect :outline :cyan))
								  ( (equal "YELLOW" color)
								    (itemconfigure canvas rect :fill :yellow)
								    (itemconfigure canvas rect :outline :cyan))
								  ( (equal "GRAY" color)
								    (itemconfigure canvas rect :fill :gray)
								    (itemconfigure canvas rect :outline :cyan))
								  (t   (itemconfigure canvas rect :fill :black)
								       (itemconfigure canvas rect :outline :cyan))) ))) ))
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
					 :command
				    (lambda () (when (consp *items*)
						  (let* ( (rem (pop *items*))
							  (elem (car rem))
							  (posix '(0 0 0 0)))
						    (set-coords canvas elem posix)))) ))
		     (save (make-instance 'button :text "Save"
					  :master menu
					  :command
				  (lambda () (nailtk::save-frame)
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
				(let* ( (rxy (pop *items*))
					(rect (car rxy))
					(xx (cadr rxy))
					(yy (caddr rxy))
					(xx1 (+ (cadddr rxy) 1))
					(yy1 (cadddr (cdr rxy))  )
					(color (car (last rxy)) ))
				  (push (list rect xx yy xx1 yy1 color)
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
					(yy1 (cadddr (cdr rxy))  )
					(color (car (last rxy)) ))
				  (push (list rect xx yy xx1 yy1 color)
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
					  (yy1 (+ (cadddr (cdr rxy)) 1))
					  (color (car (last rxy)) ))
					    
				  (push (list rect xx yy xx1 yy1 color)
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
				    (yy1 (- (cadddr (cdr rxy)) 1))
				    (color (car (last rxy)) ))
			      (push (list rect xx yy xx1 yy1 color ) *items*)
			      (set-coords canvas rect
				     (list xx yy xx1 yy1))) )))
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
			 (itemconfigure canvas rect :outline :cyan))
		       ( (equal scolor 'green)
			 (itemconfigure canvas rect :fill :green)
			 (itemconfigure canvas rect :outline :cyan))
		       ( (equal scolor 'yellow)
			 (itemconfigure canvas rect :fill :yellow)
			 (itemconfigure canvas rect :outline :cyan))
		       ( (equal scolor 'black)
			 (itemconfigure canvas rect :fill :black)
			 (itemconfigure canvas rect :outline :cyan))
		       (t  (itemconfigure canvas rect :fill :gray)
			   (itemconfigure canvas rect :outline :cyan)))
                 (when *items*
		   (itemconfigure canvas (caar *items*) :outline :white))
				  
                 (push (list rect xx yy xx1 yy1 scolor) *items*)
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
(defun main() 
  (format t "~&-----------------NailTK-------------------")
  (format t "~&Natural and artificial laboratory")
  (format t "~&Federal University of Uberlândia")
  (format t "~&Vers : ~a" *version*)
  (format t "~&May/2017")
  (read-config)
  (format t "~&Window size ~ax, ~ay~%" *xsize* *ysize*)
  (format t "~&------------------------------------------~%")
  (setf *items* nil)
  (finish-output t)
  (nailtk::stick))

(defun compile-ntk ()
  (sb-ext:save-lisp-and-die "nailtk" :executable t :toplevel #'main))
