


;; p1.41

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define inc
  (lambda (x)
    (+ x 1)))


;; p 1.42
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))
(define square
  (lambda (x)
    (* x x)))


;; p 1.43
(define repeated
  (lambda (f n)
    (if (< n 1)
        (lambda (x) x)
        (compose f (repeated f (- n 1))))))



