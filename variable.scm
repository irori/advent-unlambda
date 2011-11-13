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
   '(return
     location
     newloc
     oldlocs
     visits
     verbose    ; contains information for look_count and interval
     limit
     turns
     clock1
     clock2
     tally
     lost-treasures
     closed
     panic
     was-dark
     dwarf   ; (dloc, odloc, dseen)[5]
     dflag
     dkill
     knife-loc
     rand
     word12
     mot
     verb
     oldverb
     obj
     oldobj
     not-warned
     hinted
     hint-count
     death-count
     liquid
     default-msg
     message
     base
     prop
     place
     note)))

(define (getter-name sym) sym)
(define (setter-name sym)
  (string->symbol (string-append "set-" (symbol->string sym))))
(define (getter-with-implicit-world-name sym)
  (string->symbol (string-append "$" (symbol->string sym))))
(define (setter-with-implicit-world-name sym)
  (string->symbol (string-append "$set-" (symbol->string sym))))

(define (modifier-body plist)
  (list 'world
        (fold
         (lambda (p e)
           `(lambda (_hd _tl)
	      ,(if (eq? p 'car)
		   `((snoc _tl) ,(if e `(_hd ,e) '(modifier _hd)))
		   `((icons _hd) ,(if e `(_tl ,e) '(modifier _tl))))))
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
        (add-unl-macro! (getter-with-implicit-world-name tree) '()
			(list (getter-name tree) 'world))
        (add-unl-macro! (setter-name tree) '(world modifier)
                        (modifier-body plist))
        (add-unl-macro! (setter-with-implicit-world-name tree) '(modifier)
			(list (setter-name tree) 'world 'modifier))
	)))

(generate-accessors '() memory-map)

; (let-world (m1 .. mn) body)
; -> (let* ((world m1)
;               :
;           (world mn))
;      body)
(defsyntax (let-world modifiers body)
  `(let* ,(map (lambda (m) (list 'world m)) modifiers)
     ,body))

(defmacro (set-nth world setter n modifier)
  (setter world (modify-nth modifier (to-cons1 n))))

(defmacro ($set-nth setter n modifier)
  (set-nth world setter n modifier))

(defmacro ($place-of obj)
  (nth obj (place world)))

(defmacro ($prop-of obj)
  (nth obj (prop world)))

(defmacro (set-prop-of obj modifier world)
  (set-nth world set-prop obj modifier))

(defmacro ($set-prop-of obj modifier)
  (set-nth world set-prop obj modifier))

(defmacro ($base-of obj)
  (nth obj (base world)))

(defmacro ($set-base-of obj modifier)
  (set-nth world set-base obj modifier))

(defmacro $not-closing
  (cons1? (clock1 world)))

(defmacro (set-nth-hinted n modifier world)
  (set-nth world set-hinted n modifier))

(defmacro ($set-nth-hinted n modifier)
  (set-nth world set-hinted n modifier))
