(ql:quickload :ltk)
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

(defun main (*posix-argv*)
  ; gui main function
  (with-ltk ()
	    ; set window title
	    (wm-title *tk* "primes")

	    ; make radio buttons
	    (let* ((naive-button (make-instance 'radio-button :text "naive"
						:value "NAIVE"
						:variable "method"))
		   (miller-rabin-button (make-instance 'radio-button :text "miller-rabin"
						       :value "MILLER-RABIN"
						       :variable "method"))

		   ; make entry boxes
		   (number-of-bits (make-instance 'entry))
		   (s (make-instance 'entry))
		   (prime-to-be-checked (make-instance 'entry))

		   ; make labels
		   (number-of-bits-label (make-instance 'label :text "number of bits:"))
		   (s-label (make-instance 'label :text "s:"))
		   (prime-label (make-instance 'label :text "prime to be checked:"))
		   (result-output (make-instance 'label :text ""))
		   (time-output (make-instance 'label :text ""))

		   ; make buttons
		   (generate-prime-button (make-instance 'button :text "generate prime"))
		   (check-prime-button (make-instance 'button :text "check if prime")))

	      ; set default option for the radio buttons
	      (setf (value naive-button) "NAIVE")

	      ; naive method for testing prime numbers
	      (defun naive ()
		(setf (text result-output) (lcg (expt 2 32) 69069 0)))

	      ; miller-rabin method for testing prime numbers
	      (defun miller-rabin ()
		(setf (text result-output) (+ 1 2)))

	      ; generate random prime number based on the chosen test method
	      (defun generate-prime ()
		(funcall (intern (string (value naive-button)))))

	      (setf (command generate-prime-button) #'generate-prime)

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
  )
