

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))
(define find-divisor
  (lambda (n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (next test-divisor))))))
(define next
  (lambda (n)
    (if (= n 2)
        3
        (+ n 2))))
(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))

(define prime?
  (lambda (n)
    (= n (smallest-divisor n))))

