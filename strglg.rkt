#lang racket/base
(require racket/string
         racket/list)

(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define facts* '())

(define (into-list strg)
  (map list->string (filter (λ (x) (not (equal? x '()))) (to-list (filter (λ (x) (not (char=? x #\space))) strg) '(())))))

(define (to-list strg n) 
  (if (empty? strg) n
      (cond [(and (not (empty? (pop n))) (char=? (car (pop n)) #\"))
             (if (char=? (car strg) #\") (to-list (cdr strg) 
                                                  (append (ret-pop n) (list (append (pop n) (list #\")) '())))
                 (to-list (cdr strg) (push (ret-pop n) (push (pop n) (car strg)))))]
            [(member (car strg) (list #\( #\) #\, #\. #\? #\;)) (to-list (cdr strg) (append n (list (list (car strg)) '())))]
            [(and (> (length strg) 2) (equal? (take strg 2) (list #\: #\-))) (to-list (cddr strg) (append n (list (list #\: #\-) '())))]
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
  (if (empty? lst) (filter (λ (x) (not (equal? x ","))) n) (paren (cdr lst) (push~ n (car lst)))))

#;(define (expr lst)
  (if (and (char-alphabetic? (car (string->list (car lst)))) (list? (second lst)))
      (list (car lst))))

(define (commas lst) (cms lst '()))
(define (cms lst n) (displayln lst)
  (if (empty? lst) n
  (cond [(list? (car lst)) (cms (cdr lst) (push n (commas (cdar lst))))]
        [(string=? (car lst) ",") (append (list n) (commas (cdr lst)))]
        [else (cms (cdr lst) (push n (car lst)))])))

(define (sentence lst n)
  (if (empty? lst) n
      (cond [(equal? (car lst) ".") (sentence (cdr lst) (push (ret-pop n) (list 'statement (pop n))))]
            [(equal? (car lst) "?") (sentence (cdr lst) (push (ret-pop n) (list 'question (pop n))))]
            [(equal? (car lst) ";") (sentence (cdr lst) (push (ret-pop n) (list 'nondet (pop n))))]
            [else (sentence (cdr lst) (push n (car lst)))])))

(define (rm-commas lst) (filter (λ (x) (not (equal? x ","))) lst))
(define (infix:- lst n)
  (if (empty? lst) n
      (cond [(equal? (car lst) ":-") (let ([q (dropf lst (λ (x) (or (not (list? x)) (not (equal? (car x) 'statement)))))])
             (infix:- (cdr q)
                      (push (ret-pop n) (list ":-" (pop n)
                        (append (takef (cdr lst) (λ (x) (and (list? x)) (not (equal? (car x) 'statement))))
                                (list (car q)))))))]
            [else (infix:- (cdr lst) (push n (car lst)))])))
(define (add-statements lst) (adds lst '()))
(define (adds lst n)
  (if (empty? lst) n
      (if (and (list? (car lst)) (equal? (caar lst) 'statement)) (begin (set! facts* (push facts* (second (car lst))))
                                                                        (adds (cdr lst) n))
          (adds (cdr lst) (push n (car lst))))))

; not final definition; just quick sketch for it.
(define (questions lst) (qs lst '()))
(define (qs lst n)
  (if (empty? lst) n
      (if (and (list? (car lst)) (equal? (caar lst) 'question))
          (if (empty? (filter (λ (x) (equal? x (second (car lst)))) facts*)) 
              (qs (cdr lst) (push n #f)) (qs (cdr lst) (push n (second (car lst)))))
          (qs (cdr lst) (push n (car lst))))))

(define (valid? l) (if (empty? (filter (λ (x) (equal? x l)) facts*)) #f l))
          

(define (parse lst) ;expr is mapped because later there will be a statement list.
  (map (λ (x) (valid? (second x))) (add-statements (infix:- (sentence (parenthesize (rm-commas lst)) '()) '()))))

(define (process strg)
  (parse (into-list (string->list strg))))

(define (main)
  (write (process (read-line)))
  (main))

(main)