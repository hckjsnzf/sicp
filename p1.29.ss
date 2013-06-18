

(define cube
  (lambda (x)
    (* x x x)))

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
          (sum term (next a) next b)))))


(define integral
  (lambda (f a b dx)
    (define add-dx
      (lambda (x)
        (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
      dx)))



(define s-rule
  (lambda (f a b n)
    (let [(h (/ (- b a) (* n 1.0)))
          (k 0)]
      (* (/ h 3)
        (sum (lambda (x)
               (let ([k (cond
                          [(= x 0) 1]
                          [(= x n) 1]
                          [(odd? x) 4]
                          [else
                            2])])
                 (* k (f (+ a (* x h))))))
          0 (lambda (x)
              (+ x 1))
          n)))))
    


