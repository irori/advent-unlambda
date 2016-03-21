(define-module dwarf
  (use unlc)
  (use lib)
  )
(select-module dwarf)

(defmacro (make-dwarf loc oloc seen)
  (lambda (f) (f loc oloc seen)))
(defmacro (dloc dwarf) (dwarf (lambda (x _ _) x)))
(defmacro (odloc dwarf) (dwarf (lambda (_ x _) x)))
(defmacro (dseen dwarf) (dwarf (lambda (_ _ x) x)))

(defmacro initial-dwarf (list (make-dwarf chest-loc V V)
                              (make-dwarf hmk V V)
                              (make-dwarf wfiss V V)
                              (make-dwarf y2 V V)
                              (make-dwarf like3 V V)
                              (make-dwarf complex V V)))
(defmacro initial-dflag c0)
(defmacro initial-knife-loc c0)

(defmacro ($set-nth-dwarf n modifier)
  (set-nth world set-dwarf n modifier))
(defmacro (set-dloc x dwarf)
  (dwarf (lambda (_ oloc seen) (make-dwarf x oloc seen))))
(defmacro (set-odloc x dwarf)
  (dwarf (lambda (loc _ seen) (make-dwarf loc x seen))))
(defmacro (set-dseen x dwarf)
  (dwarf (lambda (loc oloc _) (make-dwarf loc oloc x))))

(defmacro dwarf-here?
  (lambda (world)
    (call/cc
     (lambda (q)
       (((if< $dflag c2 q I) V)
        (c5 (lambda (lst)
              (lst (lambda (dwf rest)
                     (K rest
                        ((= (dloc dwf) $location) q I)))))
            (cdr $dwarf)))))))

(defmacro $dwarf-here? (dwarf-here? world))

(defmacro kill-all-dwarves
  (lambda (world)
    ($set-dwarf (K (c6 (cons (make-dwarf limbo V V)) V)))))

(defmacro ($too-easy? i)
  (and (= i PYRAMID)
       (or (= $location proom)
           (= $location droom))))

(defmacro $pirate-not-spotted
  (zero? ($place-of MESSAGE)))
