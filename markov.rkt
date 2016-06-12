#lang racket

(provide markov-stages)

(struct markov  (lists))


(define lista '(0.20 0.30 0.5))
(define listb '(0.13 0.07 0.8))
(define listc '(0.10 0.70 0.20))
(define stages '('a 'b 'c))
(define stages2 '("a" "b" "c"))

; this function should take a markov chain and a current stage, and pass back t he same chain with a new stage
(define list-test (list '('a 0.3 0.2 0.5) '('b 0.2 0.4 0.4) '('c 0.1 0.5 0.4)))
(define test-chain (markov list-test))

(define (match-markov chain item)
  (if (empty? chain) (error 'matching "item not found in chain")
      (let ([x (first (rest chain))])
        (match x
          [chain (rest (rest chain))]
          [_ (match-markov (rest chain) item)]))))

#| (define (random-position list-of-percents list-of-stages)
  (let ([x (random 100)]
        [sorted-list (map (lambda (number) (* 100 number))
                          (sort list-of-percents <))]
        [combined-list (interpose (map (lambda (number)
                                         (* 100 number))
                                       list-of-percents)
                                  list-of-stages)])
    (if (percent-match? (flatten (add-2 sorted-list 0)) x)
        (assoc (percentify sorted-list
                           (flatten (add-2 sorted-list 0)) x) combined-list)
        (error 'position "something's gone wrong")))) |#

(define (random-position list-of-percents list-of-stages)
  (let ([x (random 100)]
        [sorted-list (map (lambda (number) (* 100 number))
                          (sort list-of-percents <))]
        [combined-list (interpose (map (lambda (number)
                                         (* 100 number))
                                       list-of-percents)
                                  list-of-stages)])
    (if (percent-match? (flatten (add-2 sorted-list 0)) x)
        (assoc (percentify sorted-list
                           (flatten (add-2 sorted-list 0)) x) combined-list)
        (error 'position "something's gone wrong"))))

(define (move-markov listpercs liststage complete-stage cur-stage)
  (if (empty? liststage) (error 'move "object doesnt exist?")
      (let ([f (first liststage)])
        (cond
          [(equal? cur-stage f) (random-position (first listpercs) complete-stage)]
          [else (move-markov (rest listpercs) (rest liststage) complete-stage cur-stage)]))))

(define (interpose xs ys)
  (match (list xs ys)
    [(list '() ys) ys]
    [(list  xs '()) xs]
    [(list (cons x xs) (cons y ys)) (cons (cons x y)(interpose xs ys))]))

(define (percent-match? list-of-percents number)
  (if (empty? list-of-percents) #f
      (if (<= number (first list-of-percents)) #t (percent-match? (rest list-of-percents) number))))

(define (percentify list-of-percs added-percs number)
  (if (empty? list-of-percs) 0
      (if (<= number (first added-percs)) (first list-of-percs)
          (percentify (rest list-of-percs) (rest  added-percs)  number))))

(define (add-2 lista prev-elem)
  (if (empty? (rest lista)) (+ (first lista) prev-elem)
      (if (= prev-elem 0) (cons (first lista)(add-2 (rest lista) (first lista)))
          (cons (+ prev-elem (first lista)) (add-2 (rest lista) (+ prev-elem (first lista)))))))

(define percent-tests '(.25 .25 .5))
(define percent2 '(25.0 25. 50.0))


;(define (return-item
#|
(define-syntax-rule markov
  (syntax-rules ()
    [(markov [a] [num])
     (error
      'markov "Not a markov chain. Arity mismatch.")]
    [(markov [a b ...]
             [num num2 ...])
     ((num num2...) (a b ...) (a b ...))]))
|#

;(define stx  #`(markov [a] [num]))

(define-syntax (move-markov! stx)
  (syntax-case stx (move-markov!)
    [(move-markov! stages percents key)
     #`(list stages percents (rest (flatten (move-markov percents stages stages key))))]))

(define (markov-stages stages percents key times)
  (if (= times 0) (first (reverse (flatten (move-markov! stages percents key))))
      (cons key (markov-stages stages percents (first (reverse (flatten (move-markov! stages percents key)))) (- times 1)))))
