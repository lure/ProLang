#lang racket

(require "hw5.rkt")
(require rackunit)

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

					(define rlist (list (var "a") (int 3)))
(define mlist (apair (var "a") (apair (int 3) (aunit))))
(check-equal?
  rlist
  (mupllist->racketlist mlist))
(check-equal?
  mlist
  (racketlist->mupllist rlist))
 
; problem 2
 
(check-equal?
  (eval-exp (add (int 1) (int 2)))
  (int 3))
(check-equal?
  (eval-exp (ifgreater (int 2) (int 1) (int 2) (int 1)))
  (int 2))
(check-equal?
  (eval-exp (ifgreater (int 1) (int 2) (int 2) (int 1)))
  (int 1))
(check-equal?
  (eval-exp (fst (apair (int 1) (int 2))))
  (int 1))
(check-equal?
  (eval-exp (snd (apair (int 1) (int 2))))
  (int 2))
(check-equal?
  (eval-exp (aunit))
  (aunit))
(check-equal?
  (eval-exp (isaunit (aunit)))
  (int 1))
(check-equal?
  (eval-exp (isaunit (int 1)))
  (int 0))
 
(define f
  (fun "sum-to"
       "x"
       (ifgreater (var "x")
                  (int 0)
                  (add (var "x")
                       (call (var "sum-to")
                             (add (var "x") (int -1))))
                  (int 0))))
(check-equal?
  (eval-exp (call f (int 5)))
  (int 15))
 
(define g
  (fun #f
       "x"
       (mlet "floor"
             (add (int 2) (int 1))
             (add (var "floor") (var "x")))))
(check-equal?
  (eval-exp (call g (int 3)))
  (int 6))
(check-equal?
  (eval-exp (mlet "x" (int 20) (var "x")))
  (int 20))
 
; problem 3
 
(check-equal?
  (eval-exp (ifaunit (int 1) (int 2) (int 3)))
  (int 3))
(check-equal?
  (eval-exp (ifaunit (aunit) (int 2) (int 3)))
  (int 2))
(check-equal?
  (eval-exp (ifaunit (mlet "u"
                           (aunit)
                           (call (fun #f
                                      "v"
                                      (ifaunit (var "v") (int 22) (var "u")))
                                 (int 33)))
                     (int 2)
                     (int 3)))
  (int 2))
 
(check-equal?
  (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (add (var "x") (int 2))))
                   (add (var "x") (var "y"))))
  (int 4))
 
(check-equal?
  (eval-exp (ifeq (int 3) (add (int 1) (int 2)) (int 1) (int 2)))
  (int 1))
(check-equal?
  (eval-exp (ifeq (int 3) (add (int 1) (int 1)) (int 1) (int 2)))
  (int 2))
 
; problem 4
 
(check-equal?
  (eval-exp (call (call mupl-map f)
                  (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))
  (apair (int 1) (apair (int 3) (apair (int 6) (aunit)))))
(check-equal?
  (eval-exp (call (call mupl-mapAddN (int 7))
                  (apair (int 3) (apair (int 4) (apair (int 9) (aunit))))))
  (apair (int 10) (apair (int 11) (apair (int 16) (aunit)))))
 