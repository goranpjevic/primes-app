(ql:quickload :ltk)
(use-package :ltk)

; seed for the lcg
(defvar *r* 1)
; linear congruential generator
(defun lcg (m a b)
  (setf *r* (mod (+ (* a *r*) b) m)))

(defun check-for-divisors (r &optional (j 3) (s (sqrt r)))
  ; check all odd divisors of r until √r
  ; return r if it's prime, otherwise nil
  (if (> j s)
    r
    (if (not (equal (mod r j) 0))
      (check-for-divisors r (+ j 2) s))))

(defun naive (random-number)
  ; naive method for testing prime numbers
  (or (check-for-divisors random-number)
      ; r is not prime, check r+2
      (naive (+ random-number 2))))

(defun random-number-in-range (a b)
  ; get random number in range [a, b]
  (mod (+ a (lcg (expt 2 32) 69069 0)) (- b (1+ a))))

(defun list-of-bits (integer)
  ; convert an integer to a list of its bits
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))))

(defun modular-exponentiation (a b n)
  ; calculate equations of the form a^b mod n
  (let ((d 1)
	(bits (list-of-bits b)))
    (loop for i from 0 to (length bits) do
	  (setq d (mod (expt d 2) n))
	  (if (equal (nth i bits) 1)
	    (setq d (mod (* d a) n))))
    d))

(defun miller-rabin (p s)
  ; miller-rabin method for testing prime numbers
  (if (<= p 3) "prime"
    (if (evenp p) "not prime"
      (labels ((find-d-k (d k)
			 ; return d and k, such that d*(2^k)=p-1
			 (if (evenp d)
			   (find-d-k (/ d 2) (1+ k))
			   (values d k)))
	       (update-x (x p k &optional (i 0))
			 ; if ∃i:a^(d*2^i)≡−1 (mod p), then p is probably prime
			 (if (and (<= i (1- k))
				  (not (equal x (1- p))))
			   (update-x (mod (expt x 2) p) p k (1+ i))
			   x)))
	(multiple-value-bind (d k) (find-d-k (1- p) 0)
	  (or (dotimes (j s)
		(let ((x (modular-exponentiation
			   (random-number-in-range 2 (- p 2))
			   d p)))
		  (if (not (or (equal x 1)
			       (equal (update-x x p k) (1- p))))
		    (return "not prime"))))
	      "probably prime"))))))

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
						      ; should be: [0,2^(n-2)-1]
						      ; shift bits so it's
						      ; between [0,2^(n-1)-1]
						      ; and the last bit is 0
						      (+ (ash (lcg (expt 2 (- n
									      2))
								   69069 0) 1)
							 ; add 2^(n-1) and 1, so
							 ; it's an odd number of
							 ; n bits
							 (expt 2 (1- n)) 1)))))

;	      (defun super-duper ()
;		; generate random number with n bits
;		(if (equal (read-from-string (text number-of-bits))
;				(+ 1 (floor (log
;					      (lcg (expt 2 32) 69069 0)
;					      2))))
;		  ; check for prime numbers based on the chosen test method
;		  (setf (text result-output) (funcall (intern (string (value
;									naive-button)))))
;		  (super-duper)))

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
	      (grid time-output 5 1 :padx 5 :pady 5))))
