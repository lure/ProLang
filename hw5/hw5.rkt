;; Programming Languages, Homework 5 https://gist.github.com/jonschoning/5063273

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(closure? e) e]
        [(int? e) e]        
        [(fun? e) (closure env e)]        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) 
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number(s)")))]  ;(begin (print v1) (print v2)(error "MUPL ifgreater applied to non-number(s)"))))]
        [(mlet? e) ; var value body
         (let ([v1 (eval-under-env (mlet-e e) env)])         
               (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        
        [(call? e) ; e => (closure + argument) => (env (nameopt formal body)) + actual
         (let ([v1 (eval-under-env (call-funexp e) env)] ;closure
               [v2 (eval-under-env (call-actual e) env)]);argument
           (if (closure? v1)
               ;we need prev bindings (8-9 at 6:12)
               (let* ([cfun (closure-fun v1)]                      
                      [cenv (closure-env v1)]
                      [fname (cons (fun-nameopt cfun) v1)]
                      [fparm (cons (fun-formal cfun) v2)]                      
                      [fenv (cons fparm
                                   (if (fun-nameopt cfun)
                                       (cons fname cenv)
                                       cenv))])
                 (eval-under-env (fun-body cfun) fenv))
           (error "MUPL call applied to non-closure")))]
        [(apair? e) (apair 
                     (eval-under-env (apair-e1 e) env) 
                     (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
          (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1) (int 1) (int 0)))]         
                
        [#t (error "bad MUPL expression")])); DEBUG [#t (begin (print e) (error "bad MUPL expression"))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst) 
      e2
      (mlet (car (car lstlst)) 
            (cdr (car lstlst)) 
            (mlet* (cdr lstlst) e2))))
      

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) ; or just (mlet (mlet (ifgreat ...
               (cons "_y"  e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))
  

;; Problem 4

(define mupl-map
  (fun #f "func" 
       (fun "f" "xs" 
            (ifaunit (var "xs")
                       (aunit)
                       (apair (call (var "func") (fst (var "xs"))) 
                              (call (var "f") (snd (var "xs"))))))))
                                              

(define mupl-mapAddN 
  (mlet "map" mupl-map
       (fun #f "i" 
            (call (var "map") 
                  (fun #f "x" (add (var "i") (var "x"))))))) ;mah brrrrain... 

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (compute-free-vars e)
  (struct res (e fvs)) ; result type of f (could also use a pair)
    (define (f e) 
    (cond [(var? e) (res e (set (var-string e)))]
          [(int? e) (res e (set))]
          [(add? e) (let ([r1 (f (add-e1 e))]
                          [r2 (f (add-e2 e))])
                      (res (add (res-e r1) (res-e r2))
                           (set-union (res-fvs r1) (res-fvs r2))))]
          [(ifgreater? e) (let ([r1 (f (ifgreater-e1 e))]
                                [r2 (f (ifgreater-e2 e))]
                                [r3 (f (ifgreater-e3 e))]
                                [r4 (f (ifgreater-e4 e))])
                            (res (ifgreater (res-e r1) (res-e r2) (res-e r3) (res-e r4))
                                 (set-union (res-fvs r1) (res-fvs r2) (res-fvs r3) (res-fvs r4))))]
          [(fun? e) (let* ([r (f (fun-body e))]
                           [fvs (set-remove (res-fvs r) (fun-formal e))]
                           [fvs (if (fun-nameopt e) (set-remove fvs (fun-nameopt e)) fvs)])
                      (res (fun-challenge (fun-nameopt e) (fun-formal e) (res-e r) fvs)
                           fvs))]
          [(call? e) (let ([r1 (f (call-funexp e))]
                           [r2 (f (call-actual e))])
                      (res (call (res-e r1) (res-e r2))
                           (set-union (res-fvs r1) (res-fvs r2))))]
          [(mlet? e) (let* ([r1 (f (mlet-e e))]
                            [r2 (f (mlet-body e))])
                       (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                            (set-union (res-fvs r1) (set-remove (res-fvs r2) (mlet-var e)))))]
          [(apair? e) (let ([r1 (f (apair-e1 e))]
                            [r2 (f (apair-e2 e))])
                      (res (apair (res-e r1) (res-e r2))
                           (set-union (res-fvs r1) (res-fvs r2))))]
          [(fst? e) (let ([r (f (fst-e e))])
                      (res (fst (res-e r))
                           (res-fvs r)))]
          [(snd? e) (let ([r (f (snd-e e))])
                      (res (snd (res-e r))
                           (res-fvs r)))]
          [(aunit? e) (res e (set))]
          [(isaunit? e) (let ([r (f (isaunit-e e))])
                          (res (isaunit (res-e r))
                               (res-fvs r)))]))
  (res-e (f e)))
                      
(define (eval-under-env-c e env) 
  (cond 
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)]
         ; call case uses fun-challenge as appropriate
         ; all other cases the same
        ...)

(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))