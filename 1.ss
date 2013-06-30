

;; exer 1.3
(define square
  (lambda (x)
    (* x x)))
(define sum-squares-two
  (lambda (x y)
    (+ (squares x)
      (squares y))))

(define sum-two-of-three
  (lambda (x y z)
    (if (>= x y)
        (sum-squares-two x (if (>= y z)
                               y
                               z))
        (sum-squares-two y (if (>= x z)
                               x
                               z)))))
