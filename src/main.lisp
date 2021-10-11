; init quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :ltk)
;(require 'asdf)
;(require 'ltk)
(use-package :ltk)

; seed for the lcg
(defparameter *r* 1)
; linear congruential generator
(defun lcg (m a b)
  (setf *r*
	(mod 
	  (+ (* a *r*)
	     b)
	  m)))

; gui main function
(with-ltk ()
	  ; set window title
	  (wm-title *tk* "primes")

	  ; make radio buttons
	  (let* ((naive-button (make-instance 'radio-button :text "naive"
					      :value :naive
					      :variable "method"))
		 (miller-rabin-button (make-instance 'radio-button :text "miller-rabin"
					    :value :miller-rabin
					    :variable "method"))
		 ; make buttons
		 (generate-prime-button (make-instance 'button :text "generate prime"))
		 (check-prime-button (make-instance 'button :text "check if prime"))

		 ; make entry boxes
		 (number-of-bits (make-instance 'entry))
		 (s (make-instance 'entry))
		 (prime-to-be-checked (make-instance 'entry))

		 ; make labels
		 (number-of-bits-label (make-instance 'label :text "number of bits:"))
		 (s-label (make-instance 'label :text "s:"))
		 (prime-label (make-instance 'label :text "prime to be checked:"))
		 (result-output (make-instance 'label))
		 (time-output (make-instance 'label)))

	    ; put gui widgets on grid
	    (grid naive-button 0 0 :padx 5 :pady 5)
	    (grid miller-rabin-button 0 1 :padx 5 :pady 5)
	    (grid number-of-bits-label 1 0 :padx 5 :pady 5)
	    (grid number-of-bits 1 1 :padx 5 :pady 5)
	    (grid s-label 2 0 :padx 5 :pady 5)
	    (grid s 2 1 :padx 5 :pady 5)
	    (grid prime-label 3 0 :padx 5 :pady 5)
	    (grid prime-to-be-checked 3 1 :padx 5 :pady 5)
	    (grid generate-prime-button 4 0 :padx 5 :pady 5)
	    (grid check-prime-button 4 1 :padx 5 :pady 5)
	    (grid result-output 5 0 :padx 5 :pady 5)
	    (grid time-output 5 1 :padx 5 :pady 5)))
