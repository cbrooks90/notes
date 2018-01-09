(define (next-rational x)
  (/ (+ (* 2 (floor x)) (- x) 1)))

(define (evaluate f x y)
  (+ (* (car f) x x)
     (* (cadr f) x y)
     (* (caddr f) y y)))

(define (represented-values f n)
  (cons (evaluate f 1 0)
        (let loop ([i 1] [x 1])
          (if (>= i n) '()
              (cons (evaluate f (numerator x) (denominator x))
                    (loop (+ i 1) (next-rational x)))))))
