

;; gcd from s1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))



(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (make-rat n d)
  (let [(g (gcd n d))]
    (cons (/ n g) (/ d g))))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;; p2.2
(define (make-segment x y)
  (cons x y))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point y)
  (cdr y))

(define (midpoint-segment seg)
  (let [(beg (start-segment seg))
        (end (end-segment seg))]
    (make-point
     (/ (+ (x-point beg)
           (x-point end))
        2.0)
     (/ (+ (y-point beg)
           (y-point end))
        2.0))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


;; p2.4
(define (y-cons x y)
  (lambda (m) (m x y)))
(define (y-car z)
  (z (lambda (p q) p)))
(define (y-cdr z)
  (z (lambda (p q) q)))



;; p2.6
(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))


 ;; p2.7
(define (make-interval a b)
  (cons a b))
(define (upper-bound val)
  (cdr val))
(define (lower-bound val)
  (car val))

(define (sub-interval x y)
  (let ([p1 (- (lower-bound x) (lower-bound y))]
        [p2 (- (lower-bound y) (lower-bound x))]
        [p3 (- (upper-bound x) (upper-bound y))]
        [p4 (- (upper-bound y) (upper-bound x))])
    (make-interval (min p1 p2)
                   (max p3 p4))))

