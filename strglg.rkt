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
(define (infix:- lst n) (displayln lst)
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
                  (valid? (second x)) 
                  (if (and (list? x) (equal? (car x) 'or))
                      (if (ormap valid? (cdr x)) (findf valid? x) #f) x))) lst))
(define (qs x)
  (if (and (list? x) (equal? (car x) 'question))
      (valid? (second x)) 
      (if (and (list? x) (equal? (car x) 'or))
        (if (ormap valid? (cdr x)) (findf valid? x) #f) x)))

(define (all-real? lst l) 
  (if (or (not (list? l)) (not (= (length (car lst)) (length l)))) #f
    (if (andmap (λ (x e) (or (equal? x e) (equal? (car (string->list x)) #\_))) (cdar lst) (cdr l)) l #f)))

#;(define (dep? lst l)
  (map (λ (x) 
    (if (not (equal? (length l) (length (car x)))) #f
        (let ([c (list (map (λ (a b) (if (equal? a b) a (if (equal? (car (string->list a)) #\_) (list a b) #f))) (cdar x) (cdr l)) (cdr x))])
          (if (not (member #f c))
              (let d? ([s '()] [ls '()])
                (map (λ (y) (if (list? y) (map (λ (z) (d? s (cdr z))) y)
                                (if (equal? (car (string->list y) #\_)) (findf (λ (q) (equal? q y)) (map second s)) y))) ls)
              (d? (filter (λ (q) (and (list? q) (= (length q) 2) (equal? (car (string->list (cadr q))) #\_))) (car c)) (cdr c))) #f)))) dep-facts*))

#;(define (sym-eq? a b)
  (or (and (list? a) (list? b) (= (length a) (length b))) ()))
(define (dep? f l) (if (or (not (list? l)) (not (= (length l) (length (car f)))) (not (all-real? f l))) #f
  (let ([d (filter (λ (x) (and (list? x) (= (length x) 2) (equal? (car (string->list (second x))) #\_)))
             (map (λ (x y) (if (equal? (car (string->list x)) #\_) (list y x) 
                               (if (equal? x y) x #f))) (cdar f) (cdr l)))])
    (andmap qs (let repl ([ls (second f)])
      (cond [(list? ls) (map repl ls)]
            [(and (string? ls) (equal? (car (string->list ls)) #\_)) (car (findf (λ (q) (equal? (second q) ls)) d))]
            [else ls]))))))
                  

(define (valid? l) (if (empty? (filter (λ (x) (equal? x l)) facts*)) 
                       (if (empty? (filter (λ (x) (dep? x l)) dep-facts*)) 
                           (cond  [(all-real? (list (list 'full "_a" "Prn") '(a)) l)
                                   (begin (displayln (second l)) l)]
                                  [(all-real? (list (list 'full "_a" "_b" "Eq") '(a)) l)
                                   (if (equal? (second l) (third l)) l #f)]
                                  [else #f]) l) l))

(define (add-df lst) (ad lst '()))
(define (ad lst n)
  (if (empty? lst) n
      (if (and (list? (car lst)) (equal? (caar lst) ":-")) 
          (begin (set! dep-facts* (push dep-facts* 
                                        (push (ret-pop (cdar lst)) 
                                              (push (ret-pop (second (cdar lst))) 
                                                (cond [(and (list? (second (pop (second (cdar lst))))) (equal? (car (second (pop (second (cdar lst))))) 'or) (equal? (car (pop (second (cdar lst)))) 'statement)) 
                                                       (second (pop (second (cdar lst))))]
                                                      [(equal? (car (pop (second (cdar lst)))) 'statement) (list 'question (second (pop (second (cdar lst)))))])))))
                 (ad (cdr lst) n)) (ad (cdr lst) (push n (car lst))))))

(define (cm-ors lst) (coms lst '()))
(define (coms lst n)
  (if (empty? lst) n
      (cond [(and (list? (car lst)) (equal? (caar lst) 'nondet)
                  (or (empty? n) (not (list? (pop n))) (and (list? (pop n)) (not (equal? (car (pop n)) 'nondet)))))
             (coms (cdr lst) (push n (list 'nondet (cadar lst))))]
            [(and (list? (car lst)) (equal? (caar lst) 'nondet))
             (coms (cdr lst) (push (ret-pop n) (push (pop n) (cadar lst))))]
            [(and (list? (car lst)) (not (equal? (caar lst) 'nondet))
                  (not (empty? n)) (list? (pop n)) (equal? (car (pop n)) 'nondet))
             (coms (cdr lst) (push (ret-pop n) 
                                   (if (equal? (caar lst) 'statement)
                                       (list 'statement (append (list 'or) (cdr (push (pop n) (cadar lst)))))
                                       (append (list 'or) (cdr (push (pop n) (cadar lst)))))))]
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