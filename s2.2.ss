
;; p 2.17

(define (last-pair lst)
  (cond
    [(null? lst) #f]
    [(null? (cdr lst)) lst]
    [else (last-pair (cdr lst))]))

;; p 2.18
(define (m-reverse lst)
  (define (ftmp lst olst)
    (if (null? lst)
        olst
        (ftmp (cdr lst) (cons (car lst) olst))))
  (ftmp lst '()))

;; p 2.20
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



;; p 2.21
(define (square-list lst)
  (if (null? lst)
      '()
      (cons (* (car lst) (car lst))
        (square-list (cdr lst)))))
(define (square-list-m lst)
  (map (lambda (x) (* x x))
    lst))

;; p 2.23
(define (for-each-m f lst)
  (if (null? lst)
      (begin
        (newline)
        #t)
      (begin
        (f (car lst))
        (for-each-m f (cdr lst)))))
  
