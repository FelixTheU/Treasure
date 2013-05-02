; the queens question and show all the style it can be like in the chess board.

(define (safe? k pos)
    (let ( (x (car pos)) (he (+ k (car pos))) (cha (- k (car pos))))
      (define (in col  seq)
        (cond ((null? seq) #t)
          ((let ((row (car seq)))
             (not (or (= x row)
                      (= (- col row) cha)
                      (= (+ col row) he))))
           (in (- col 1) (cdr seq)))
          (else #f)))
      (in (- k 1) (cdr pos))))




; (safe? 8 '(6 4 1 5 8 2 7 3))


(define (filter pred seq)
    (cond ((null? seq) '())
      ((pred (car seq))
       (cons (car seq)
         (filter pred (cdr seq))))
      (else (filter pred (cdr seq)))))



(define (string-double str)
    (string-append str str))



(define (string-extend str num)
    (cond ((= num 1) str)
      ((even? num) (string-extend (string-double str) (/ num 2)))
      (else (string-append str 
              (string-extend (string-double str) (/ (- num 1) 2))))))



(define (string-change-char str numb char)
    (string-set! str numb char)
    str)




(define (show-queens size layout-seq)

    (let ((init-line (string-extend (string-extend "_" 6) size))
          (fir-line (string-extend "     |" size))
          (third-line (string-extend "_____|" size)))
      (display init-line)
      (newline)
      (for-each (lambda (x)
                  (let ((sec-line (string-change-char (string-copy fir-line) 
                                    (+ 2 (* 6 (- x 1)))
                                    #\#)))
                    (display fir-line)
                    (newline)
                    (display sec-line)
                    (newline)
                    (display third-line)
                    (newline)))
        layout-seq)))


;(show-queens 8 '(6 4 1 5 8 2 7 3))




(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))



(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
          (accumulate op initial (cdr seq)))))



(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))




(define (adjoin-position new-row k seq)
    (cons new-row seq))



(define (queens board-size)
    (define (queen-cols k)
      (if (= k 0)
          (list '())
          (filter
            (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                       (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
              (queen-cols (- k 1))))))
    (queen-cols board-size))



(define (show-all-queens size)
  (for-each (lambda (x)
              (show-queens size x))
    (queens size)))

