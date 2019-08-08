#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")

; Union of two lists l1 and l2

(define (union l1 l2)
(define (remove-duplicates ls)
  (set->list (list->set ls)))
  (remove-duplicates (append l1 l2)))

; Position of a tree 

(define (TreePos t)
  (cond [(Epsilon? t) (Epsilon-n t)]
        [(Literal? t) (Literal-n t)]
        [(Or? t) (Or-n t)]
        [(Then? t) (Then-n t)]
        [(Star? t) (Star-n t)]))

; bulidNullable Helper Function

(define (buildNullable-helper t)
  (cond [(Epsilon? t) #t]
        [(Literal? t) #f]
        [(Or? t) (or (buildNullable-helper (Or-t1 t)) (buildNullable-helper (Or-t2 t)))]
        [(Then? t) (and (buildNullable-helper (Then-t1 t)) (buildNullable-helper (Then-t2 t)))]
        [(Star? t) #t]))

; buildFirst Helper Function

(define (buildFirst-helper t)
  (cond [(Epsilon? t) (list (Epsilon-n t))]
        [(Literal? t) (list (Literal-n t) (Literal-n t))]
        [(Or? t) (let* ([l1 (buildFirst-helper (Or-t1 t))]
                        [l2 (buildFirst-helper (Or-t2 t))])
                   (cons (Or-n t) (sort (union (cdr l1) (cdr l2)) <)))]
        [(Then? t) (let* ([l1 (buildFirst-helper (Then-t1 t))]
                          [l2 (buildFirst-helper (Then-t2 t))])
                     (if ;(member (cons (TreePos (Then-t1 t)) #t) nullable)
                         (buildNullable-helper (Then-t1 t))
                         (cons (TreePos t) (sort (union (cdr l1) (cdr l2)) <))
                         (cons (TreePos t) (sort (cdr l1) <))))]
        [(Star? t) (cons (TreePos t) (sort (cdr (buildFirst-helper (Star-t t))) < ))]))

; buildLast Helper Function

(define (buildLast-helper t)
  (cond [(Epsilon? t) (list (Epsilon-n t))]
        [(Literal? t) (list (Literal-n t) (Literal-n t))]
        [(Or? t) (let* ([l1 (buildLast-helper (Or-t1 t))]
                        [l2 (buildLast-helper (Or-t2 t))])
                   (cons (Or-n t) (sort (union (cdr l1) (cdr l2)) <)))]
        [(Then? t) (let* ([l1 (buildLast-helper (Then-t1 t))]
                          [l2 (buildLast-helper (Then-t2 t))])
                     (if ;(member (cons (TreePos (Then-t2 t)) #t) nullable)
                         (buildNullable-helper (Then-t2 t))
                         (cons (TreePos t) (sort (union (cdr l1) (cdr l2)) <))
                         (cons (TreePos t) (sort (cdr l2) <))))]
        [(Star? t) (cons (TreePos t) (sort (cdr (buildLast-helper (Star-t t))) <))]))

; buildNullable 

(define (buildNullable t)
  (cond [(Epsilon? t) (list (cons (Epsilon-n t) #t))]
        [(Literal? t) (list (cons (Literal-n t) #f))]
        [(Or? t) (append (buildNullable (Or-t1 t)) (list (cons (Or-n t) (buildNullable-helper t))) (buildNullable (Or-t2 t)))]
        [(Then? t) (append (buildNullable (Then-t1 t)) (list (cons (Then-n t) (buildNullable-helper t))) (buildNullable (Then-t2 t)))]
        [(Star? t) (append (buildNullable (Star-t t)) (list (cons (Star-n t) #t)))]))

; buildFirst

(define (buildFirst t)
  (cond [(Epsilon? t) (list (buildFirst-helper t))]
        [(Literal? t) (list (buildFirst-helper t))]
        [(Or? t) (append (buildFirst (Or-t1 t)) (list (buildFirst-helper t)) (buildFirst (Or-t2 t)))]
        [(Then? t) (append (buildFirst (Then-t1 t)) (list (buildFirst-helper t)) (buildFirst (Then-t2 t)))]
        [(Star? t) (append (list (buildFirst-helper t)) (buildFirst (Star-t t)))]))

; bulidLast

(define (buildLast t)
  (cond [(Epsilon? t) (list (buildLast-helper t))]
        [(Literal? t) (list (buildLast-helper t))]
        [(Or? t) (append (buildLast (Or-t1 t)) (list (buildLast-helper t)) (buildLast (Or-t2 t)))]
        [(Then? t) (append (buildLast (Then-t1 t)) (list (buildLast-helper t)) (buildLast (Then-t2 t)))]
        [(Star? t) (append (list (buildLast-helper t)) (buildLast (Star-t t)))]))

; buildFollow

(define (buildFollow t)
  ;;
  (define (buildFollow-helper-1 t)
    (cond [(Epsilon? t) null]
          [(Literal? t) null]
          [(Or? t) (append (buildFollow-helper-1 (Or-t1 t)) (buildFollow-helper-1 (Or-t2 t)))]
          [(Then? t) (append (buildFollow-helper-1 (Then-t1 t)) (list (cons (TreePos (Then-t1 t)) (TreePos (Then-t2 t)))) (buildFollow-helper-1 (Then-t2 t)))]
          [(Star? t) (buildFollow-helper-1 (Star-t t))]))
  ;;
  (define (buildFollow-helper-2 t)
    (cond [(Epsilon? t) null]
          [(Literal? t) null]
          [(Or? t) (append (buildFollow-helper-2 (Or-t1 t)) (buildFollow-helper-2 (Or-t2 t)))]
          [(Then? t) (append (buildFollow-helper-2 (Then-t1 t)) (buildFollow-helper-2 (Then-t2 t)))]
          [(Star? t) (cons (TreePos (Star-t t)) (buildFollow-helper-2 (Star-t t)))]))
  ;;
  (define (buildFollow-helper-3 l)
    (cond [(null? l) null]
          [else (let* ([l1 (extract (caar l) lastpos)]
                       [l2 (extract (cdar l) firstpos)])
                  (append (f (cdr l1) (cdr l2)) (buildFollow-helper-3 (cdr l))))]))
  ;;
  (define (buildFollow-helper-4 l)
    (cond [(null? l) null]
          [else (let* ([l1 (extract (car l) lastpos)]
                       [l2 (extract (car l) firstpos)])
                  (append (f (cdr l1) (cdr l2)) (buildFollow-helper-4 (cdr l))))]))
  ;;
  (define (buildFollow-helper-5 t)
    (cond [(Epsilon? t) null]
          [(Literal? t) (list (Literal-n t))]
          [(Or? t) (append (buildFollow-helper-5 (Or-t1 t)) (buildFollow-helper-5 (Or-t2 t)))]
          [(Then? t) (append (buildFollow-helper-5 (Then-t1 t)) (buildFollow-helper-5 (Then-t2 t)))]
          [(Star? t) (buildFollow-helper-5 (Star-t t))]))
  ;;
  (define (f l1 l2)
    (cond [(null? l1) null]
          [else (append (list (cons (car l1) l2)) (f (cdr l1) l2))]))
  ;;
  (define (merge l)
    (cond [(null? l) null]
          [else (append (cdar l) (merge (cdr l)))]))
  ;;
  (define (remove-duplicates ls)
    (set->list (list->set ls)))
  ;;
  (define (extract ele l)
    (cond [(null? l) ele]
          [else (if (= ele (car (car l))) (car l) (extract ele (cdr l)))]))
  ;;
  (define (filter-list ele l)
    (filter (lambda(x) (= ele (car x))) l))
  ;;
  (define (filter-and-merge ele l)
    (cons ele (sort (remove-duplicates (merge (filter-list ele l))) <)))
  ;;
  (define lastpos (buildLast t))
  (define firstpos (buildFirst t))
  (define l1 (buildFollow-helper-1 t))
  (define l2 (buildFollow-helper-2 t))
  (define ans-1 (buildFollow-helper-3 l1))
  (define ans-2 (buildFollow-helper-4 l2))
  (define list-literals (buildFollow-helper-5 t))
  ;;
  (filter (lambda(x) (not (null? (cdr x))))
  (map (lambda(x) (filter-and-merge x (append ans-1 ans-2))) list-literals)))

; buildGraph

(define (buildGraph r)
  (define t (maketree r))
  ;;
  (define (extract ele l)
    (cond [(null? l) ele]
          [else (if (= ele (car (car l))) (car l) (extract ele (cdr l)))]))
  ;;
  (define (position-literal t)
    (cond [(Epsilon? t) null]
          [(Literal? t) (list (cons (Literal-n t) (Literal-c t)))]
          [(Or? t) (append (position-literal (Or-t1 t)) (position-literal (Or-t2 t)))]
          [(Then? t) (append (position-literal (Then-t1 t)) (position-literal (Then-t2 t)))]
          [(Star? t) (position-literal (Star-t t))]))
  ;;
  (define (remove-duplicates ls) (set->list (list->set ls)))
  ;;
  (define (extract-pos symbol state)
    (filter (lambda(x) (member (cons x symbol) pos-lit)) state))
  ;;
  (define (union-followpos positions)
    (remove-duplicates (append* (map (lambda(x) (cdr (extract x followpos))) positions))))
  ;;
  (define (for-each-sym state)
    (map (lambda(x) (cons x (list (sort (union-followpos (extract-pos x state)) <)))) list-symbols))
  ;;
  (define (pop stack)
    (car stack))
  ;;
  (define (push states stack nodes)
    (cond [(null? states) stack]
          [(or (member (cadar states) nodes) (member (cadar states) stack) (null? (cadar states))) (push (cdr states) stack nodes)]
          [else (push (cdr states) (append stack (list (cadar states))) nodes)]))
  ;;
  (define (process-edges edges)
    (filter (lambda(x) (not (or (equal? (Trans-sym x) "#") (equal? (Trans-final x) null)))) edges))
  ;;
  (define (buildrednodes ele nodes)
    (cond [(null? nodes) null]
          [(member ele (car nodes)) (cons (car nodes) (buildrednodes ele (cdr nodes)))]
          [else (buildrednodes ele (cdr nodes))]))
  ;;
  (define (buildGraph-helper edges nodes stack)
    (cond [(null? stack) (list (process-edges edges) (reverse nodes))]
          [else (let* ([top (pop stack)]
                       [pop-stack (cdr stack)]
                       [states (for-each-sym top)]
                       [newedges (append (map (lambda(x) (Trans top (car x) (cadr x))) states) edges)]
                       [newnodes (cons top nodes)]
                       [newstack (push states pop-stack newnodes)])
                  (buildGraph-helper newedges newnodes newstack))]))                    
  ;;
  (define pos-lit (position-literal t))
  (define end-pos
    (caar (filter (lambda(x) (equal? "#" (cdr x))) pos-lit)))
  (define firstpos (buildFirst t))
  (define followpos (cons (list end-pos) (buildFollow t)))
  (define root (TreePos t))
  (define greenstate (cdr (extract root firstpos)))
  (define list-symbols (reverse (remove-duplicates (map (lambda(x) (cdr x)) pos-lit))))
  (define ans1 (buildGraph-helper null null (list greenstate)))
  (define edges (car ans1))
  (define nodes (cadr ans1))
  (define rednodes (buildrednodes end-pos nodes))
  (Graph greenstate nodes edges rednodes list-symbols))

;(define r "(a|b)*bba | cc*")
;(define r "@|((a|b)*bba | cc*)")
;(define r "abbbca|(d|@)*")
;(define r "((a*)*|(abc*d*)*a)*")
;(define r "(a*|b)*")
;(define r "((abcdef)*|(@|d)*)*")

;(define t (maketree r))