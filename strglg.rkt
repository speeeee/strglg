#lang racket/base
(require racket/string
         racket/list)

(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))

(define dep-facts* '())
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

(define (underscores lst) (map (λ (x) (if (equal? (first (string->list x)) #\_) (list x "var") x)) lst))

(define (push~ n s)
  (if (empty? n) (if (equal? s "(") (push n '()) (push n s))
  (cond [(and (list? (pop n)) (or (empty? (pop n)) (not (equal? (car (pop n)) 'full))))
         (push (ret-pop n) (push~ (pop n) s))]
        [(equal? s "(") (push n '())]
        [(equal? s ")") (append (list 'full) n)]
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
                      (push (ret-pop n) (list ":-" (pop n) #;(filter (λ (x) (not (equal? (car (string->list x)) #\_))) (cdr (pop n)))
                                              #;(filter (λ (x) (equal? (car (string->list x)) #\_)) (cdr (pop n)))
                        (append (takef (cdr lst) (λ (x) (and (list? x)) (not (equal? (car x) 'statement))))
                                (list (car q)))))))]
            [else (infix:- (cdr lst) (push n (car lst)))])))
(define (add-statements lst) (adds lst '()))
(define (adds lst n)
  (if (empty? lst) n
      (if (and (list? (car lst)) (equal? (caar lst) 'statement)) (begin (set! facts* (push facts* (second (car lst))))
                                                                        (adds (cdr lst) n))
          (adds (cdr lst) (push n (car lst))))))

(define (q lst)
  (map (λ (x) (if (and (list? x) (equal? (car x) 'question))
                  (valid? (second x)) x)) lst))

(define (dep? lst l)
  (if (andmap (λ (x e) (or (equal? x e) (equal? (car (string->list x)) #\_))) (cdar lst) (cdr l)) l #f))

(define (valid? l) (if (empty? (filter (λ (x) (equal? x l)) facts*)) 
                       (if (empty? (filter (λ (x) (dep? x l)) dep-facts*)) #f l) l))

(define (add-df lst) (ad lst '()))
(define (ad lst n)
  (if (empty? lst) n
      (if (and (list? (car lst)) (equal? (caar lst) ":-")) (begin (set! dep-facts* (push dep-facts* (cdar lst)))
                                                                  (ad (cdr lst) n)) (ad (cdr lst) (push n (car lst))))))

(define (cm-ors lst) (coms lst '()))
(define (coms lst n) (displayln lst)
  (if (empty? lst) n
      (cond [(and (list? (car lst)) (equal? (caar lst) 'nondet)
                  (or (empty? n) (and (list? (pop n)) (not (equal? (car (pop n)) 'nondet)))))
             (coms (cdr lst) (push n (list 'nondet (cadar lst))))]
            [(and (list? (car lst)) (equal? (caar lst) 'nondet))
             (coms (cdr lst) (push (ret-pop n) (push (pop n) (cadar lst))))]
            [(and (list? (car lst)) (not (equal? (caar lst) 'nondet))
                  (not (empty? n)) (list? (pop n)) (equal? (car (pop n)) 'nondet))
             (coms (cdr lst) (push (ret-pop n) (append (list 'or) (cdr (push (pop n) (cadar lst))))))]
            [else (coms (cdr lst) (push n (car lst)))])))

(define (parse lst) ;expr is mapped because later there will be a statement list.
  (q (add-df (add-statements (infix:- (cm-ors (sentence (parenthesize (rm-commas lst)) '())) '())))))

(define (process strg)
  (parse (into-list (string->list strg))))

(define (main)
  (write (process (read-line)))
  (write dep-facts*)
  (main))

(main)