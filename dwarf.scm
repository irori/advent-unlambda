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
(defmacro initial-not-dkill I)  ; have you killed a dwarf?

(defmacro dwarf-here?
  (lambda (world)
    (call/cc
     (lambda (q)
       (((if< $dflag 2 q I) V)
        (c5 (lambda (lst)
              (lst (lambda (dwf rest)
                     (K rest
                        ((= (dloc dwf) $location) q I)))))
            (cdr $dwarf)))))))
