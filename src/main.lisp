(ql:quickload :ltk)
(use-package :ltk)

; seed for the lcg
(defvar *r* 1)
; linear congruential generator
(defun lcg (m a b)
  (setf *r* (mod (+ (* a *r*) b) m)))

(defun naive (r &optional (j 3) (s (sqrt r)))
  ; check all odd divisors of r until √r
  ; return r if it's prime, otherwise nil
  (if (> j s)
    r
    (if (not (equal (mod r j) 0))
      (naive r (+ j 2) s))))

(defun generate-random-prime-naive (random-number)
  ; use naive method for testing prime numbers
  (or (naive random-number)
      ; r is not prime, check r+2
      (generate-random-prime-naive (+ random-number 2))))

(defun random-number-in-range (a b)
  ; get random number in range [a, b]
  (mod (+ a (lcg (expt 2 32) 69069 0)) (- b (1+ a))))

(defun modular-exponentiation (a b n &optional (i (integer-length b)) (d 1))
  ; calculate a^b mod n
  (if (> i 0)
    (if (logbitp (1- i) b)
      (modular-exponentiation a b n (1- i) (mod (* (mod (expt d 2) n) a) n))
      (modular-exponentiation a b n (1- i) (mod (expt d 2) n)))
    d))

(defun miller-rabin (p s)
  ; miller-rabin primality test
  ; return r if it's prime, otherwise nil
  (if (<= p 3) p
    (if (not (evenp p))
      (labels ((find-d-k (d k)
			 ; return d and k, such that d*(2^k)=p-1
			 (if (evenp d)
			   (find-d-k (/ d 2) (1+ k))
			   (values d k)))
	       (update-x (x p k &optional (i 0))
			 ; if ∃i:a^(d*2^i)≡−1 (mod p), then p is probably prime
			 (if (or (> i (1- k))
				 (equal x (1- p)))
			   x
			   (update-x (mod (expt x 2) p) p k (1+ i)))))
	(multiple-value-bind (d k) (find-d-k (1- p) 0)
	  (or (dotimes (j s)
		(let ((x (modular-exponentiation
			   (random-number-in-range 2 (- p 2))
			   d p)))
		  (if (not (or (equal x 1)
			       (equal (update-x x p k) (1- p))))
		    (return-from miller-rabin nil))))
	      p))))))

(defun generate-random-prime-miller-rabin (random-number s)
  ; use miller-rabin method for testing prime numbers
  (or (miller-rabin random-number s)
      ; r is not prime, check r+2
      (generate-random-prime-miller-rabin (+ random-number 2) s)))

(defun main (*posix-argv*)
  ; gui main function
  (with-ltk ()
	    ; set window title
	    (wm-title *tk* "primes")

	    ; make all ui elements
	    (let ((naive-button (make-instance 'radio-button :text "naive"
					       :value "NAIVE"
					       :variable "method"))
		  (miller-rabin-button (make-instance 'radio-button :text "miller-rabin"
						      :value "MILLER-RABIN"
						      :variable "method"))

		  (number-of-bits-entry (make-instance 'entry))
		  (s-entry (make-instance 'entry))
		  (prime-to-be-checked-entry (make-instance 'entry))

		  (number-of-bits-label (make-instance 'label :text "number of bits:"))
		  (s-label (make-instance 'label :text "s:"))
		  (prime-label (make-instance 'label :text "prime to be checked:"))
		  (result-output (make-instance 'label :text ""))
		  (result-label (make-instance 'label :text "result:"))
		  (time-output (make-instance 'label :text ""))
		  (time-label (make-instance 'label :text "time [μs]:"))

		  (generate-prime-button (make-instance 'button :text "generate prime"))
		  (check-prime-button (make-instance 'button :text "check if prime")))

	      (defun generate-prime ()
		(let ((n (read-from-string (text number-of-bits-entry)))
		      (s (read-from-string (text s-entry))))
		  ; generate random odd number of n bits
		  (let ((random-number (+ (ash (lcg (expt 2 (- n 2)) 69069 0) 1)
					  (expt 2 (1- n)) 1))
			(start-time (get-internal-real-time)))
		    (setf (text result-output)
			  (if (string= (string (value naive-button)) "NAIVE")
			    (generate-random-prime-naive random-number)
			    (generate-random-prime-miller-rabin random-number s)))
		    (setf (text time-output) (- (get-internal-real-time) start-time)))))

	      (defun check-prime ()
		(let ((r (read-from-string (text prime-to-be-checked-entry)))
		      (s (read-from-string (text s-entry))))
		  (let ((start-time (get-internal-real-time)))
		    (setf (text result-output)
			  (if (string= (string (value naive-button)) "NAIVE")
			    (if (naive r) "prime" "not prime")
			    (if (miller-rabin r s) "probably prime" "not prime")))
		    (setf (text time-output) (- (get-internal-real-time) start-time)))))

	      ; set default option for the radio buttons
	      (setf (value naive-button) "NAIVE")

	      ; call generate-prime, when generate-prime-button is clicked
	      (setf (command generate-prime-button) #'generate-prime)
	      (setf (command check-prime-button) #'check-prime)

	      ; put gui widgets on grid
	      (grid naive-button 0 0 :padx 5 :pady 5)
	      (grid miller-rabin-button 0 1 :padx 5 :pady 5)
	      (grid number-of-bits-label 1 0 :padx 5 :pady 5)
	      (grid number-of-bits-entry 1 1 :padx 5 :pady 5)
	      (grid s-label 2 0 :padx 5 :pady 5)
	      (grid s-entry 2 1 :padx 5 :pady 5)
	      (grid prime-label 3 0 :padx 5 :pady 5)
	      (grid prime-to-be-checked-entry 3 1 :padx 5 :pady 5)
	      (grid generate-prime-button 4 0 :padx 5 :pady 5)
	      (grid check-prime-button 4 1 :padx 5 :pady 5)
	      (grid result-label 5 0 :padx 5 :pady 5)
	      (grid result-output 5 1 :padx 5 :pady 5)
	      (grid time-label 6 0 :padx 5 :pady 5)
	      (grid time-output 6 1 :padx 5 :pady 5))))
