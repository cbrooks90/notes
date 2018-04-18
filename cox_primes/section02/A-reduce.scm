(define (minimize-b a b c)
  (cond [(and (< b 0) (< b (- a)))
         (let ([m (ceiling (/ (+ a b) (* -2 a)))])
           (switch-a-c a (+ b (* 2 a m)) (+ (* (+ (* a m) b) m) c)))]
        [(and (>= b 0) (> b a))
         (let ([m (ceiling (/ (- b a) (* 2 a)))])
           (switch-a-c a (- b (* 2 a m)) (+ (* (- (* a m) b) m) c)))]
        [else (switch-a-c a b c)]))

(define (switch-a-c a b c)
  (cond [(> a c) (minimize-b c (- b) a)]
        [(> (abs b) a) (minimize-b a b c)]
        [else (finalize a b c)]))

(define (finalize a b c)
  (cond [(>= b 0) (list a b c)]
        [(= a (- b)) (list a a c)]
        [(= a c) (list a (- b) c)]
        [else (list a b c)]))

(define (reduced-pos-def-forms D)
  (define (aux a bs)
    (let loop ([bs bs])
      (if (null? bs) '()
          (let-values ([(q r) (div-and-mod (- (* (car bs) (car bs)) D) (* 4 a))])
            (cond [(> r 0) (loop (cdr bs))]
                  [(> (gcd a (car bs) q) 1) (loop (cdr bs))]
                  [else (cons (list a (car bs) q)
                              (if (or (= a q) (= a (car bs)) (zero? (car bs)))
                                  (loop (cdr bs))
                                  (cons (list a (- (car bs)) q) (loop (cdr bs)))))])))))
  (let ([as (cdr (iota (+ 1 (exact (floor (sqrt (/ (- D) 3)))))))])
    (let loop ([as as] [bs (list (if (even? D) 0 1))])
      (if (null? as) '()
          (append
            (aux (car as) bs)
            (loop (cdr as) (if (= (car as) (car bs)) bs (cons (+ (car bs) 2) bs))))))))

(define (discriminant a b c)
  (- (* b b) (* 4 a c)))

(define tests
  '(( 79 24   2) ( 79 -24   2) (  2 24  79) (  2 -24  79)
    (158 24   1) (158 -24   1) (  1 24 158) (  1 -24 158)
    (  3 26  61) (  3 -26  61) ( 61 26   3) ( 61 -26   3)
    (183 26   1) (183 -26   1) (  1 26 183) (  1 -26 183)
    ( 70 28   3) ( 70 -28   3) (  3 28  70) (  3 -28  70)
    ( 30 28   7) ( 30 -28   7) (  7 28  30) (  7 -28  30)
    ( 15 28  14) ( 15 -28  14) ( 14 28  15) ( 14 -28  15)
    (  5 28  42) (  5 -28  42) ( 42 28   5) ( 42 -28   5)
    ( 10 28  21) ( 10 -28  21) ( 21 28  10) ( 21 -28  10)
    (  2 28 105) (  2 -28 105) (105 28   2) (105 -28   2)
    ( 35 28   6) ( 35 -28   6) (  6 28  35) (  6 -28  35)
    (239 30   1) (239 -30   1) (  1 30 239) (  1 -30 239)))

; Make sure every quadratic form in this list is one of
; '(1 0 14), '(2 0 7), '(3 2 5), '(3 -2 5).
; These are the 4 equivalence classes of discriminant -56.
(define test-results
  (map (lambda (x) (apply minimize-b x)) tests))
