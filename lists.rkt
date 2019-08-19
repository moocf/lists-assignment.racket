#lang racket

; (duple n x): N X N -> List(N)
; returns a list containing n copies of x
; : n=0  -> ()
; : else -> (x . (duple n-1 x))
(define duple
  (lambda (n x)
    (if (<= n 0)
        (list)
        (cons x (duple (- n 1) x)))))


; (invert lst): List(N X N) -> List(N X N)
; returns a list with each 2-list reversed
; : size(lst)=0 -> ()
; : else        -> ((lst[0,1] lst[0,0]) (invert lst[1..end]))
(define invert
  (lambda (lst)
    (if (null? lst)
        (list)
        (cons (list (cadar lst) (caar lst)) (invert (cdr lst))))))


; (down lst): List(N) -> List(List)
; wraps parentheses around each top-level element of lst
; : size(lst)=0 -> ()
; : else        -> ((lst[0]) (down lst[1..end]))
(define down
  (lambda (lst)
    (if (null? lst)
        (list)
        (cons (list (car lst)) (down (cdr lst))))))


; (swapper.sym s1 s2 sym): S X S X S -> S
; returns s2 if sym=s1, s1 if sym=s2, else sym
(define swapper.sym
  (lambda (s1 s2 sym)
    (if (eq? sym s1)
        s2
        (if (eq? sym s2)
            s1
            sym))))

; (swapper.sexp s1 s2 sexp): S X S X SE -> SE
; returnes s-exp with all occurrences of s1 replaced with s2
(define swapper.sexp
  (lambda (s1 s2 sexp)
    (if (or (symbol? sexp) (number? sexp))
        (swapper.sym s1 s2 sexp)
        (swapper s1 s2 sexp))))

; (swapper s1 s2 slist): S X S X SL -> SL
; returns s-list with all occurrences of s1 replaced with s2
; : size(slist)=0 -> ()
; : else          -> ((swapper.sexp slist[0]) . (swapper slist[1..end]))
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        (list)
        (cons (swapper.sexp s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))))


; (list-set lst n x): L X N X N -> L
; returns list with n-th element replaced by x
; : size(lst)=0 -> ()
; : n=0         -> (x . lst[1..end])
; : else        -> (lst[0] . (list-set lst[1..end] n-1 x))
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (list)
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))


; (count-occurrences.sexp s sexp): S X SE -> N
; returns number of occurrences of s in sexp
(define count-occurrences.sexp
  (lambda (s sexp)
    (if (or (symbol? sexp) (number? sexp))
       (if (eq? sexp s)
           1
           0)
       (count-occurrences s sexp))))

; (count-occurrences s slist): S X SL -> N
; returns occurrences of s in slist
; : size(slist)=0 -> 0
; : else          -> (count-occurrences.sexp s lst[0]) + (count-occurrences s lst[1..end])
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences.sexp s (car slist)) (count-occurrences s (cdr slist))))))


; (product.sym sym sos): S X SL -> LL
; returns cartesian product of sym with sos
(define product.sym
  (lambda (sym sos)
    (if (null? sos)
        (list)
        (cons (list sym (car sos)) (product.sym sym (cdr sos))))))

; (product sos1 sos2): SL X SL -> LL
; returns cartesian product of sos1 with sos2
; : size(sos1)=0 -> ()
; : else         -> (append (product.sym sos1[0] sos2) (product sos1[1..end] sos2))
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        (list)
        (append (product.sym (car sos1) sos2) (product (cdr sos1) sos2)))))


; (filter-in pred lst): F X L -> L
; returns list of elements in lst that satisfy the predicate pred
; : size(lst)=0   -> ()
; : else if pred? -> (lst[0] . (filter-in pred lst[1..end]))
; : else          -> (filter-in pred lst[1..end])
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        (list)
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))


; (list-index.pos pred lst pos): F X L X N -> N
; returns pos-based position of first element in lst that satisfies predicate pred
(define list-index.pos
  (lambda (pred lst pos)
    (if (null? lst)
        #f
        (if (pred (car lst))
            pos
            (list-index.pos pred (cdr lst) (+ pos 1))))))

; (list-index pred lst): F X L -> N
; returns 0-based position of first element in lst that satisfies predicate pred
(define list-index
  (lambda (pred lst)
    (list-index.pos pred lst 0)))


; (every? pred lst): F X L -> B
; returns #f if any element of lst fails to satisfy pred, #t otherwise
; : size(lst)=0      -> #t
; : (pred lst[0])=#t -> (every pred lst[1..end])
; : else             -> #f
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))


; (exists? pred lst): F X L -> B
; returns #t if any element of lst satisfies pred, #f otherwise
; : size(lst)=0      -> #f
; : (pred lst[0])=#t -> #t
; : else             -> (exists? pred lst[1..end])
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))


; (up.sexp sexp to)
; removes a pair of parentheses from s-exp to list "to"
; : sexp=list -> (sexp .. to) (append)
; : else      -> (sexp . to)
(define up.sexp
  (lambda (sexp to)
    (if (list? sexp)
        (append sexp to)
        (cons sexp to))))

; (up.lst lst to): L X L -> L
; removes a pair of parentheses from each top-level element of lst to list "to"
; : lst=null -> to
; : else     -> (up.sexp lst[0] (up.lst lst[1..end] to))
(define up.lst
  (lambda (lst to)
    (if (null? lst)
        to
        (up.sexp (car lst) (up.lst (cdr lst) to)))))

; (up lst): L -> L
; removes a pair of parentheses from each top-level element of lst
(define up
  (lambda (lst)
    (up.lst lst (list))))

; (flatten.sexp sexp to): SE X L -> L
; return list of symbols in sexp added to list "to"
(define flatten.sexp
  (lambda (sexp to)
    (if (not (list? sexp))
        (cons sexp to)
        (flatten.slist sexp to))))

; (flatten.slist slist to): SL X L -> L
; returns list of symbols in slist added to list "to"
(define flatten.slist
  (lambda (slist to)
    (if (null? slist)
        to
        (flatten.sexp (car slist) (flatten.slist (cdr slist) to)))))

; (flatten slist): SL -> L
; returns list of symbols in slist in the same order
(define flatten
  (lambda (slist)
    (flatten.slist slist (list))))


; (merge.lst pred loi1 loi2 to): F X L X L X L -> L
; merges loi1 and loi2 using pred to list "to"
; : loi1=null & loi2=null                             -> to
; : loi2=null | (loi1!=null & (pred loi1[0] loi2[0])) -> (loi1[0] . (merge.lst pred loi1[1..end] loi2 to))
; : else                                              -> (loi2[0] . (merge.lst pred loi1 loi2[1..end] to))
(define merge.lst
  (lambda (pred loi1 loi2 to)
    (if (and (null? loi1) (null? loi2))
        to
        (if (or (null? loi2) (and (not (null? loi1)) (pred (car loi1) (car loi2))))
            (cons (car loi1) (merge.lst pred (cdr loi1) loi2 to))
            (cons (car loi2) (merge.lst pred loi1 (cdr loi2) to))))))

; (merge loi1 loi2): L X L -> L
; merge loi1 and loi2 in ascending order
(define merge
  (lambda (loi1 loi2)
    (merge.lst <= loi1 loi2 (list))))


; (sort.lst pred loi to): F X L X L -> L
; returns list of elements sorted by pred to list "to"
; (this is insertion sort as it merges a single number with a sorted list of numbers)
(define sort.lst
  (lambda (pred loi to)
    (if (null? loi)
        to
        (merge.lst pred (list (car loi)) (sort.lst pred (cdr loi) (list)) to))))

; (sort lst): L -> L
; returns a list of elements sorted in ascending order
(define sort
  (lambda (loi)
    (sort.lst <= loi (list))))


; (sort/predicate pred loi): F X L -> L
; returns a list of elements sorted by predicate
(define sort/predicate
  (lambda (pred loi)
    (sort.lst pred loi (list))))
