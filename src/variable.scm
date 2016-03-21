(define-module variable
  (use unlc)
  (use lib)
  (export memory-map make-initial-map)
  )
(select-module variable)

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

(define memory-map-old
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
     was-dark
     dwarf   ; (dloc, odloc, dseen)[5]
     dflag
     dkill-panic
     knife-loc
     rand
     west-count
     foobar
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
     base
     prop
     place
     note)))

(define memory-map
  (cons (cons (cons (cons (cons 'word12 (cons 'hint-count 'oldverb)) (cons (cons 'clock2 'dflag) 'mot)) 'obj) 'place) (cons (cons 'location (cons (cons (cons 'hinted (cons 'foobar 'knife-loc)) (cons (cons (cons 'note 'tally) 'return) 'oldlocs)) (cons (cons 'limit (cons (cons 'liquid 'death-count) (cons 'visits 'verbose))) 'rand))) (cons 'prop (cons (cons 'base (cons (cons 'dkill-panic (cons (cons 'not-warned 'oldobj) 'was-dark)) 'newloc)) (cons (cons 'dwarf (cons 'lost-treasures (cons (cons 'turns 'west-count) 'clock1))) 'verb))))))

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
;                        (string-append "{" (symbol->string (getter-name tree)) "}"))
        (add-unl-macro! (getter-with-implicit-world-name tree) '()
			(list (getter-name tree) 'world))
        (add-unl-macro! (setter-name tree) '(world modifier)
                        (modifier-body plist))
;                        (string-append "<" (symbol->string (setter-name tree)) ">"))
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
  (setter world (update-nth modifier n)))

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

(defmacro $not-closed
  (cons1? (clock2 world)))

(defmacro $closed?
  (not $not-closed))

(defmacro (set-nth-hinted n modifier world)
  (set-nth world set-hinted n modifier))

(defmacro ($set-nth-hinted n modifier)
  (set-nth world set-hinted n modifier))


(define (make-initial-map tree)
  (if (pair? tree)
      `(icons ,(make-initial-map (car tree))
              ,(make-initial-map (cdr tree)))
      (string->symbol (string-append "initial-" (symbol->string tree)))))

(defmacro initial-return V)
(defmacro initial-location road)
(defmacro initial-newloc road)
(defmacro initial-oldlocs (icons road road))
(defmacro initial-verbose (to-cons1 c4))
(defmacro initial-limit (to-cons1 c330))
(defmacro initial-turns V)
(defmacro initial-clock1 (to-cons1 c15))
(defmacro initial-clock2 (to-cons1 c30))
(defmacro initial-tally c15)
(defmacro initial-lost-treasures c0)
(defmacro initial-dkill-panic V)
(defmacro initial-was-dark V)
(defmacro initial-west-count (to-cons1 c10))
(defmacro initial-foobar c0)
(defmacro initial-word12 V)
(defmacro initial-mot V)
(defmacro initial-verb V)
(defmacro initial-oldverb V)
(defmacro initial-not-warned I)
(defmacro initial-hinted (list c2 c2 c2 c4 c5 c3  ; hint2-7
                               c5 c10  ; hint0, hint1
                               c4  ; gave_up
                               c0))  ; bonus
(defmacro initial-hint-count V)
(defmacro initial-death-count c0)
(defmacro initial-obj V)
(defmacro initial-oldobj V)
(defmacro initial-dummy V)
