

(define fixed-point
  (lambda (f first-guess)
    (define tolerance 0.00001)
    (define close-enough?
      (lambda (v1 v2)
        (< (abs (- v1 v2)) tolerance)))
    (define try
      (lambda (guess)
        (let ([next (f guess)])
          (if (close-enough? guess next)
              next
              (try next)))))
    (try first-guess)))


(define average
  (lambda (x y)
    (/ (+ x y) 2.0)))



(define dis-fixed-point
  (lambda (f first-guess)
    (define tolerance 0.00001)
    (define close-enough?
      (lambda (v1 v2)
        (< (abs (- v1 v2)) tolerance)))
    (define try
      (lambda (guess)
        (let ([next (f guess)])
          (display guess)
          (newline)
          (if (close-enough? guess next)
              next
              (try next)))))
    (try first-guess)))




;; p1.37
(define cont-frac
  (lambda (fn fd k)
    (define frac-inside
      (lambda (x)
        (if (= x k)
            (/ (fn x) (fd x))
            (/ (fn x) (+ (fd x) (frac-inside (+ x 1)))))))
    (frac-inside 1)))
;; b
(define cont-frac-i
  (lambda (fn fd k)
    (define frac-i
      (lambda (init k)
        (if (= k 1)
            (/ (fn 1) (+ (fd 1) init))
            (frac-i (/ (fn k) (+ (fd k) init)) (- k 1)))))
    (frac-i 0 k)))


;; p1.38
(define get-e
  (lambda (k)
    (+ 2 (cont-frac-i (lambda (x) 1.0)
           (lambda (x)
             (cond
               [(= (remainder x 3) 2) (* 2 (+ 1 (quotient x 3)))]
               [else 1]))
           k))))
