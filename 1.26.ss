


(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))
(define find-divisor
  (lambda (n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1))))))
(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))

(define prime?
  (lambda (n)
    (= n (smallest-divisor n))))

(define expmod
  (lambda (base exp m)
    (cond
      [(= exp 0) 1]
      [(even? exp)
       (remainder (square (expmod base (/ exp 2) m))
         m)]
      [else
        (remainder (* base (expmod base (- exp 1) m))
          m)])))
(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))
(define fast-prime?
  (lambda (n times)
    (cond
      [(= times 0) #t]
      [(fermat-test n)
       (fast-prime? n (- times 1))]
      [else
        #f])))


