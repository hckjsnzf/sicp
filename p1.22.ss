


(define search-for-primes
  (lambda (beg end)
    (cond
      [(>= beg end)
       #f]
      [(even? beg)
       (search-for-primes (+ beg 1) end)]
      [else
        (begin
          (if (prime? beg)
              (display " *** ")
              (search-for-primes (+ beg 2) end)))])))


