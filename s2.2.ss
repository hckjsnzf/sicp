
;; s 2.17

(define (last-pair lst)
  (cond
    [(null? lst) #f]
    [(null? (cdr lst)) lst]
    [else (last-pair (cdr lst))]))

(define (m-reverse lst)
  (define (ftmp lst olst)
    (if (null? lst)
        olst
        (ftmp (cdr lst) (cons (car lst) olst))))
  (ftmp lst '()))


(define (same-parity lst)
  (define (sf val lst)
    (if (null? lst)
        '()
        (if (= (remainder (car lst) 2) val)
            (cons (car lst) (sf val (cdr lst)))
            (sf val (cdr lst)))))
  (if (null? lst)
      '()
      (sf ((lambda (x)
             (remainder x 2))
           (car lst)) (cdr lst))))


(define (same-p x . lst)
  (let ([val (remainder x 2)])
    (define (sf lst)
      (if (null? lst)
          '()
          (if (= val (remainder (car lst) 2))
              (cons (car lst) (sf (cdr lst)))
              (sf (cdr lst)))))
    (sf lst)))



