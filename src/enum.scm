(use gauche.sequence)
(require "unlc.scm")
(require "lib.scm")

(define enum-table (make-hash-table))

(define (define-enum symbols :optional (start 0))
  (for-each-with-index
   (lambda (i sym)
     (add-unl-macro! sym '() (churchnum (+ start i)))
     (hash-table-put! enum-table sym (+ start i)))
   symbols))

(define (lookup-enum sym)
  (hash-table-get enum-table sym))


;; motion
(define-enum
  '(N SOUTH E W NE SE NW SW
    U D L R IN OUT FORWARD BACK
    OVER ACROSS UPSTREAM DOWNSTREAM
    ENTER CRAWL JUMP CLIMB LOOK CROSS
    ROAD WOODS VALLEY HOUSE
    GULLY STREAM DEPRESSION ENTRANCE CAVE
    ROCK SLAB BED PASSAGE CAVERN
    CANYON AWKWARD SECRET BEDQUILT RESERVOIR
    GIANT ORIENTAL SHELL BARREN BROKEN DEBRIS VIEW FORK
    PIT SLIT CRACK DOME HOLE WALL HALL ROOM FLOOR
    STAIRS STEPS COBBLES SURFACE DARK LOW OUTDOORS
    Y2 XYZZY PLUGH PLOVER OFFICE NOWHERE))

;; object
(define-enum
  '(NOTHING KEYS LAMP GRATE GRATE_
    CAGE ROD ROD2 TREADS TREADS_
    BIRD DOOR PILLOW SNAKE CRYSTAL CRYSTAL_ TABLET CLAM OYSTER
    MAG DWARF KNIFE FOOD BOTTLE WATER OIL
    MIRROR MIRROR_ PLANT PLANT2 PLANT2_
    STALACTITE SHADOW SHADOW_
    AXE ART PIRATE DRAGON DRAGON_
    BRIDGE BRIDGE_ TROLL TROLL_ TROLL2 TROLL2_
    BEAR MESSAGE GEYSER PONY BATTERIES MOSS
    GOLD DIAMONDS SILVER JEWELS COINS CHEST EGGS TRIDENT VASE
    EMERALD PYRAMID PEARL RUG RUG_ SPICES CHAIN))

;; action
(define-enum
  '(ABSTAIN TAKE DROP OPEN CLOSE ON OFF WAVE CALM GO RELAX
    POUR EAT DRINK RUB TOSS
    WAKE FEED FILL BREAK BLAST KILL
    SAY READ FEEFIE BRIEF FIND INVENTORY SCORE QUIT))

;; location
(define-enum
  '(limbo
    road hill house valley forest woods slit outside
    inside cobbles debris awk bird spit
    emist nugget efiss wfiss wmist
    like1 like2 like3 like4 like5 like6 like7
    like8 like9 like10 like11 like12 like13 like14
    brink elong wlong
    diff0 diff1 diff2 diff3 diff4 diff5
    diff6 diff7 diff8 diff9 diff10
    pony cross hmk west south ns y2 jumble windoe
    dirty clean wet dusty complex
    shell arch ragged sac ante witt
    bedquilt cheese soft
    e2pit w2pit epit wpit
    narrow giant block immense falls steep
    abovep sjunc tite low crawl window
    oriental misty alcove proom droom
    slab abover mirror res
    scan1 scan2 scan3 secret
    wide tight tall boulders
    scorr swside
    dead0 dead1 dead2 dead3 dead4 dead5 dead6
    dead7 dead8 dead9 dead10 dead11
    neside corr fork warm view chamber lime fbarr barr
    neend swend
    crack neck lose cant climb check snaked
    thru duck sewer upnout didit
    ppass pdrop troll))


;; utilities
(define (make-boolean-list enums)
  (make-lookup-table (map (lambda (x) (cons x 'I)) enums)))

(define (make-lookup-table alist)
  (if (null? alist)
      'V
      (let* ((max (apply max (map (.$ lookup-enum car) alist)))
	     (vec (make-vector (+ max 1) 'V)))
	(for-each
	 (lambda (p)
	   (vector-set! vec (lookup-enum (car p)) (cdr p)))
	 alist)
	(compress-list (vector->list vec)))))

(define (compress-list lst :optional (keep-trailing-Vs #f))
  (if (null? lst)
      'V
      (let ((e (car lst)))
        (receive (head rest) (span (lambda (x) (equal? x e)) lst)
          (if (and (not keep-trailing-Vs) (eq? e 'V) (null? rest))
              'V
              (let ((n (length head))
                    (r (compress-list rest keep-trailing-Vs)))
                (if (= n 1)
                    `(icons ,e ,r)
                    `(,(churchnum n) (icons ,e) ,r))))))))
