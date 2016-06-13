#lang racket

(require "markov.rkt")
(provide makelily)


(define pitches '(#\a #\b #\c #\d #\e #\f #\g))
(define pitch-num '(1 2 3 4 5 6 7 8 9 10 11 12))
(define chroma '("is" "" "es"))
(define octave '(3 4 5))
(define measure 32)
(define aminor (list "a'" "b'" "c'"  "d'"  "e'" "f'"  "g'"  "a''"))
(define listas (list '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);1
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);2 
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);3
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);4
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);5
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);6
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);7
                     '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);8
                    ; '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);9
                    ; '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);10
                    ; '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);11
                    ; '(0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125);12
                     ))


(define (prob num)
  (let ([init (+  1 (random num))])
    (if (= init num) #t #f)))


(define (reduce func list)
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))

(define (generate-pitches number)
  (cond
    [(= 0 number) empty]
    [else
     (cons (random 35) (generate-pitches (- number 1)))]))

(define (generate-durations number)
    (cond
    [(= 0 number) empty]
    [else
     (cons (random 6) (generate-durations (- number 1)))]))

(define ht (make-hash))
(hash-set! ht 0 "a" )
(define stlist '("a'" "ais'" "b'" "c'" "cis'" "d'" "dis'" "e'" "f'" "fis'" "g'" "gis'" "a''" "ais''" "b''" "c''" "cis''" "d''" "dis''" "e''" "f''" "fis''" "g''" "gis''" "a'''" "ais'''" "b'''" "c'''" "cis'''" "d'''" "dis'''" "e'''" "f'''" "fis'''" "g'''" "gis'''"))
(define (hash-popu)
  (map (curry hash-set! ht) (range 0 36) stlist))

 (define (num-to-pitch number)
   (hash-ref ht number))

(define (num-to-dur number)
  (cond
    [(= number 0) 1]
    [(= number 1) 2]
    [(= number 2) 4]
    [(= number 3) 8]
    [(= number 4) 16]
    [(= number 5) 32]))

;(string-join (zipit (numpitch (generate-pitches 10)) (numdur (generate-durations 10))) " ")

(define (reset-dur duration)
  (let ([i (random 6)])
    (if (= duration 1)
        (cond
          [(= i 0) 1]
          [(= i 1) 2]
          [(= i 2) 4]
          [(= i 3) 8]
          [(= i 4) 16]
          [(= i 5) 32])
          duration)))

(define (less-wholes d-set)
  (cond
    [(empty? d-set) empty]
    [(empty? (rest d-set)) d-set]
    [(let ([i (reset-dur (first d-set))])
       (cons i (less-wholes (rest d-set))))]))

(define (numpitch p-set)
  (cond
    [(empty? p-set) empty]
    [else
     (let ([init (~a (num-to-pitch (first p-set)))])
       (cons init (numpitch (rest p-set))))]))

(define (numdur p-set)
  (cond
    [(empty? p-set) empty]
    [else
     (let ([init (~a (reset-dur (num-to-dur (first p-set))))])
       (cons init (numdur (rest p-set))))]))

(define (zipit lista listb)

   (cond
    [(and (empty? lista)(empty? listb)) empty]
    [else
     (let ([i (cons (first lista) (first listb))])
       (cons i (zipit (rest lista) (rest listb))))]))


(define (make-set-chords pitch)
  (if (prob 7)
      (reduce string-append (flatten (cons (cons "<" (reduce string-append (flatten (map (curryr cons " ") (numpitch (generate-pitches (+ 1 (random 4)))))))) ">")))
      pitch))

(define (make-dotted duration)
  (if (prob 8)
      (reduce string-append (flatten (cons duration ".")))
      duration))

(define (makedots d-set)
  (map make-dotted d-set))

(define (makechords p-set)
  (map make-set-chords p-set))

(define (noteinfo x y)
  (reduce string-append (flatten (map (curryr cons " ") (zipit (makechords (numpitch (generate-pitches x))) (makedots (numdur (generate-durations y))))))))

(define (noteinfo-markov x y stringa)
  (reduce string-append (flatten
                         (map
                          (curryr cons " ")
                          (zipit
                           (flatten (markov-stages aminor listas stringa (- x 1)))
                           (makedots (numdur (generate-durations y))))))))


(define headings "\\header {
title = \"Guitar Exercises\"
composer = \"Harrison Montgomery\"
}
")

(define versions "\\version \"2.18.2\"")

(define (makepitch x y stringa)
  (hash-popu)
  (string-append "\\score {<< \\new Staff \\with {\\remove \"Bar_engraver\"} \\absolute {" (noteinfo-markov x y stringa) "}\n>>}"  ))

(define (makelily x y stringa)
  (string-append headings (string-append (makepitch x y stringa) (makepitch x y stringa) (makepitch x y stringa) (makepitch x y stringa)) versions))

