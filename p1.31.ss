

(define producti
  (lambda (f a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (* result (f a))))))
    (iter a 1)))
(define product
  (lambda (f a next b)
    (if (> a b)
        1
        (* (f a) (product f (next a) next b)))))

(define factorial-by-producti
  (lambda (x)
    (producti (lambda (x) x)
      1 (lambda (x) (+ x 1)) x)))

(define get-pi
  (lambda (x)
    (if (< x 1)
        #f
        (* 4.0
          (/
            (producti (lambda (x)
                        (cond
                          [(= x 1) 2]
                          [(even? x) (+ x 2)]
                          [else
                            (+ x 1)]))
              1 (lambda (x) (+ x 1)) x)
            (producti (lambda (x)
                        (cond
                          [(= x 1) 3]
                          [(even? x) (+ x 1)]
                          [else
                            (+ x 2)]))
              1 (lambda (x) (+ x 1)) x))))))


