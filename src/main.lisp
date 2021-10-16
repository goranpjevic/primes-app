(ql:quickload :ltk)
(use-package :ltk)

; seed for the lcg
(defvar *r* 1)
; linear congruential generator
(defun lcg (m a b)
  (setf *r*
	(mod 
	  (+ (* a *r*)
	     b)
	  m)))

(defun naive (random-number)
  ; naive method for testing prime numbers
  (if (evenp random-number)
    (naive (1+ random-number))
    (let ((j 3)
	  (sqr (sqrt random-number)))
      (loop while (and (not (equal (mod random-number j) 0))
		       (<= j sqr))
	    do
	    (setq j (+ j 2)))
      (if (> j sqr)
	(eval random-number)
	(naive (+ random-number 2))))))

; miller-rabin method for testing prime numbers
(defun miller-rabin (random-number)
  random-number)

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

	      (defun generate-prime ()
		; get random number of number_of_bits bits
		(let ((n (read-from-string (text number-of-bits))))
		  ; check for prime numbers based on the chosen test method
		  (setf (text result-output) (funcall (intern (string (value
									naive-button)))
						      ; range for random numbers
						      ; generated with lcg
						      ; should be: [0,2^(n-1)-1]
						      (+ (lcg (expt 2 (1- n))
							      69069 0)
							 ; add 2^(n-1)
							 (expt 2 (1- n)))))))

	      (defun super-duper ()
		; generate random number with n bits
		(if (equal (read-from-string (text number-of-bits))
				(+ 1 (floor (log
					      (lcg (expt 2 32) 69069 0)
					      2))))
		  ; check for prime numbers based on the chosen test method
		  (setf (text result-output) (funcall (intern (string (value naive-button)))))
		  (super-duper)))

	      ; set default option for the radio buttons
	      (setf (value naive-button) "NAIVE")

	      ; call generate-prime, when generate-prime-button is clicked
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
