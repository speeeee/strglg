#lang racket/base
(require racket/string
         racket/list)

(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define (into-list strg)
  (map list->string (filter (λ (x) (not (equal? x '()))) (to-list (filter (λ (x) (not (char=? x #\space))) strg) '(())))))

(define (to-list strg n)
  (if (empty? strg) n
      (cond [(and (not (empty? (pop n))) (char=? (car (pop n)) #\"))
             (if (char=? (car strg) #\") (to-list (cdr strg) 
                                                  (append (ret-pop n) (list (append (pop n) (list #\")) '())))
                 (to-list (cdr strg) (push (ret-pop n) (push (pop n) (car strg)))))]
            [(or (char=? (car strg) #\()
                 (char=? (car strg) #\))
                 (char=? (car strg) #\,)) (to-list (cdr strg) (append n (list (list (car strg)) '())))]
            [(char=? (car strg) #\") (to-list (cdr strg) (append n (list (list #\"))))]
            [else (to-list (cdr strg) (push (ret-pop n) (push (pop n) (car strg))))])))

(define (push~ n s)
  (if (empty? n) (if (string=? s "(") (push n '()) (push n s))
  (cond [(and (list? (pop n)) (or (empty? (pop n)) (not (equal? (car (pop n)) 'full))))
         (push (ret-pop n) (push~ (pop n) s))]
        [(string=? s "(") (push n '())]
        [(string=? s ")") (append (list 'full) n)]
        [else (push n s)])))

(define (parenthesize lst)
  (paren lst '()))
(define (paren lst n)
  (if (empty? lst) n (paren (cdr lst) (push~ n (car lst)))))

#;(define (expr lst)
  (if (and (char-alphabetic? (car (string->list (car lst)))) (list? (second lst)))
      (list (car lst))))

(define (commas lst) (cms lst '()))
(define (cms lst n) (displayln lst)
  (if (empty? lst) n
  (cond [(list? (car lst)) (cms (cdr lst) (push n (commas (cdar lst))))]
        [(string=? (car lst) ",") (append n (list (commas (cdr lst))))]
        [else (cms (cdr lst) (push n (car lst)))])))

(define (parse lst) ;expr is mapped because later there will be a statement list.
  (commas (parenthesize lst)))

(define (process strg)
  (parse (into-list (string->list strg))))

(define (main)
  (write (process (read-line))))

(main)