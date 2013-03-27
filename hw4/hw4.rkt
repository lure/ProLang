
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (if (<= low high) (cons low (sequence (+ low stride) high stride)) '()))


;(define (seq low high stride)
;  (letrec ([buildseq (lambda (bttm acc) (if (<= bttm high) (buildseq (+ bttm stride) (cons acc bttm)) (cons acc '())))][builseq (+ 3 0)])
;        (buildseq low '())))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))
  
(define (list-nth-mod xs n)
  (cond ([< n 0] (error "list-nth-mod: negative number"))
        ([null? xs] (error "list-nth-mod: empty list"))
        ([cdr xs] (let ([i (remainder n (length xs))])                    
                    (car (list-tail xs i))))))
          
(define (stream-for-n-steps s n)
  (if (> n 0) 
      (let ([pair (s)]) 
                (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))
      '()))

; Dan wants natural started from 1 instead of 0. 
(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons (if (= (remainder x 5) 0) (- x) x)
                      (lambda () (f (+ x 1)))))])
           (lambda () (f 1))))


(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))

(define (stream-add-zero s)
  (letrec ([f (lambda (xs) 
                (let ([pair (xs)])
                       (cons 
                        (cons 0 (car pair)) 
                        (lambda () (f (cdr pair))))))])
    (lambda () (f s))))
                                              
                                             
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons 
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))
                 

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (if (= len n) 
                    #f 
                    (let ([pr (vector-ref vec n)])
                         (if (and (pair? pr) (= (car pr) v))
                                    (vector-ref vec n)
                                    (f (+ n 1))))))])
    (f 0)))
                   
(define (cached-assoc xs n)
  (letrec ([values (vector n #f)]
           [pos 0]           
           [f (lambda (v)
             (let ([res (vector-assoc v values)])
                   [if res 
                       res 
                       (let ([res2 (assoc v xs)])                         
                         (if res2 (begin
                                      (set! pos (if (>= pos (- n 1)) 0 (+ pos 1)))
                                      (vector-set! values pos res2)
                                      res2)
                             res2 ))]))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([test (- e1 1)])
       (letrec ([loop (lambda ()
                        (if (< test e2) #t (loop)))
                      
                ])
         
         (loop))
       )]))