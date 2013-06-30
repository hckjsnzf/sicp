
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
  



;; s 2.2.2
;; p 2.24
;; '(1 (2 (3 4)))

;; p 2.25
;; '(1 3 (5 7) 9) -> cadaddr -> 7


;; p 2.26

;; (append x y) -> '(1 2 3 4 5 6)
;; (cons x y) -> '((1 2 3) . (4 5 6))
;; (list x y) -> '((1 2 3) (4 5 6))


;; p 2.27
(define (deep-reverse tree)
  (define (df tree lst)
    (if (null? tree)
        lst
        (df (cdr tree)
          (cons (if (pair? (car tree))
                    (df (car tree) '())
                    (car tree)) lst))))
  (df tree '()))


;; p 2.28
(define (fringe tree)
  (if (null? tree)
      '()
      (if (pair? (car tree))
          (append (fringe (car tree))
            (fringe (cdr tree)))
          (cons (car tree)
            (fringe (cdr tree))))))

(define (fringe-m tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (list tree)]
    [else
      (append (fringe-m (car tree)) (fringe-m (cdr tree)))]))



;; p 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (car mob))
(define (right-branch mob)
  (car (cdr mob)))
(define (branch-length bran)
  (car bran))
(define (branch-structure bran)
  (car (cdr bran)))

(define (total-weight mob)
  (define (weight-branch branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (weight-branch (left-branch mob))
    (weight-branch (right-branch mob))))

(define (balance?-mobile mob)
  (define (weight-branch branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (define (scale-branch branch)
    (* (branch-length branch)
      (weight-branch branch)))
  (and (if (pair? (branch-structure (left-branch mob)))
           (balance?-mobile
             (branch-structure (left-branch mob)))
           #t)
       (if (pair? (branch-structure (right-branch mob)))
           (balance?-mobile
             (branch-structure (right-branch mob)))
           #t)
       (= (scale-branch (left-branch mob))
         (scale-branch (right-branch mob)))))


;; p 2.30
(define (square-tree tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (* tree tree)]
    [else
      (cons (square-tree (car tree))
        (square-tree (cdr tree)))]))
(define (square-tree-m tree)
  (map (lambda (x)
         (cond
           [(not (pair? x)) (* x x)]
           [else
             (square-tree-m x)]))
    tree))


;; p 2.31
(define (tree-map f tree)
  (map (lambda (x)
         (cond
           [(not (pair? x)) (f x)]
           [else
             (tree-map f x)]))
    tree))

;; p 2.32
(define (subsets s)
  (if (null? s)
      '(())
      (let [(rest (subsets (cdr s)))]
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))




;; s 2.23
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
        (accumulate op initial (cdr sequence)))))
;; p 2.33
(define (m-map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
    '() sequence))
(define (m-append seq1 seq2)
  (accumulate cons
    seq2 seq1))
(define (m-length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
    0 sequence))


;; p 2.34
(define (horner-eval x sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                  (* x higher-terms)))
    0
    sequence))

;; p 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                    (fringe-m t))))
(define (count-leaves-m t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves-m node)
                             1))
                    t)))


;; p 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x)
                                       (car x))
                                  seqs))
        (accumulate-n op init (map (lambda (x)
                                     (cdr x))
                                seqs)))))


;; p 2.37 ???????
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
    m))
(define (transpose mat)
  (accumulate-n cons '() mat))




;; p 2.38
(define fold-right-m accumulate)
(define (fold-left-m op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
          (cdr rest))))
  (iter init sequence))

(fold-right-m / 1 '(1 2 3))
;; -> 3/2
(fold-left-m / 1 '(1 2 3))
;; -> 1/6
(fold-right-m list '() '(1 2 3))
;; -> (1 (2 (3)))
(fold-left-m list '() '(1 2 3))
;; -> (((() 1) 2) 3)

;; p 2.39
(define (reverse-1 sequence)
  (fold-right-m (lambda (x y)
                  (append y (list x)))
    '() sequence))
(define (reverse-2 sequence)
  (fold-left-m (lambda (x y)
                 (cons y x))
    '() sequence))



(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
;; 1.26.ss file
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap
        (lambda (i)
          (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
    sequence))
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                   (permutations (remove x s))))
        s)))


;; p 2.40
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(define (prime-sum-pairs-sim n)
  (map make-pair-sum
    (filter prime-sum?
      (unique-pairs n))))

;; p 2.41
(define (three-pairs n)
  (flatmap (lambda (n) (map (lambda (x)
                              (cons n x))
                         (unique-pairs (- n 1))))
    (enumerate-interval 1 n)))
(define (sum-of-three n k)
  (filter (lambda (x)
            (= (fold-right-m + 0 x)
              k))
    (three-pairs n)))

