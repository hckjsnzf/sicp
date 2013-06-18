

(define sumi
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a))))))
    (iter a 0)))

