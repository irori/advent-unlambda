(define (extend-memory-map tree lst)
  (cond ((null? lst) '())
        ((pair? (car tree))
         (extend-memory-map (cdr tree)
                            (extend-memory-map (car tree) lst)))
        (else
         (set-car! tree (cons (car tree) (car lst)))
         (if (null? (cdr lst))
             '()
             (begin
               (set-cdr! tree (cons (cdr tree) (cadr lst)))
               (cddr lst))))))

(define (make-memory-map lst)
  (let loop ((tree (cons (car lst) (cadr lst)))
             (lst (cddr lst)))
    (if (null? lst)
        tree
        (loop tree (extend-memory-map tree lst)))))

(define memory-map
  (make-memory-map
   '(location
     newloc
     oldlocs
     visits
     rand
     word12
     mot
     verb
     default-msg
     message
     base
     prop
     place
     objname
     note
     long-desc
     short-desc)))

(define (getter-name sym) sym)
(define (setter-name sym)
  (string->symbol (string-append "set-" (symbol->string sym))))

(define (modifier-body plist)
  (list 'world
        (fold
         (lambda (p e)
           `(lambda (_hd _tl)
              ((cons)
               ,(if (eq? p 'car) (if e `(_hd ,e) '(modifier _hd)) '_hd)
               ,(if (eq? p 'cdr) (if e `(_tl ,e) '(modifier _tl)) '_tl))))
         #f plist)))

;; generates accessor macros for the memory map
(define (generate-accessors plist tree)
  (if (pair? tree)
      (begin
        (generate-accessors (cons 'car plist) (car tree))
        (generate-accessors (cons 'cdr plist) (cdr tree)))
      (begin
        (add-unl-macro! (getter-name tree) '(world)
                        (fold-right list 'world plist))
        (add-unl-macro! (setter-name tree) '(world modifier)
                        (modifier-body plist)))))

(generate-accessors '() memory-map)