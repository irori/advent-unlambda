(require "enum.scm")

(define min-treasure (lookup-enum 'GOLD))
(define (treasure? t) (>= (lookup-enum t) min-treasure))
(define max-obj (lookup-enum 'CHAIN))

(define object-base (make-vector (+ 2 max-obj)))
(define object-prop (make-vector (+ 1 max-obj)))
(define object-place (make-vector (+ 1 max-obj)))
(define object-name (make-vector (+ 1 max-obj)))
(define object-note (make-vector (+ 1 max-obj)))

(define (new-obj obj name base loc . notes)
  (let ((n (lookup-enum obj)))
    (if name (vector-set! object-name n name))
    (vector-set! object-base n (if base (lookup-enum base) 0))
    (vector-set! object-note n notes)
    (vector-set! object-prop n (if (treasure? obj) -1 0))
    (vector-set! object-place n (lookup-enum loc))))

;; two-part objects
(new-obj 'RUG_ #f 'RUG 'scan3)
(new-obj 'RUG "Persian rug" 'RUG 'scan1
         "There is a Persian rug spread out on the floor!"
         "The dragon is sprawled out on a Persian rug!!")
(new-obj 'TROLL2_ #f 'TROLL2 'limbo)
(new-obj 'TROLL2 #f 'TROLL2 'limbo
         "The troll is nowhere to be seen.")
(new-obj 'TROLL_ #f 'TROLL 'neside)
(new-obj 'TROLL #f 'TROLL 'swside
         "A burly troll stands by the bridge and insists you throw him a\n\
treasure before you may cross."
         "The troll steps out from beneath the bridge and blocks your way."
         #f)
(new-obj 'BRIDGE_ #f 'BRIDGE 'neside)
(new-obj 'BRIDGE #f 'BRIDGE 'swside
         "A rickety wooden bridge extends across the chasm, vanishing into the\n\
mist.  A sign posted on the bridge reads, \"STOP!  PAY TROLL!\""
         "The wreckage of a bridge (and a dead bear) can be seen at the bottom\n\
of the chasm.")
(new-obj 'DRAGON_ #f 'DRAGON 'scan3)
(new-obj 'DRAGON #f 'DRAGON 'scan1
         "A huge green fierce dragon bars the way!"
         "Congratulations!  You have just vanquished a dragon with your bare\n\
hands! (Unbelievable, isn't it?)"
         "The body of a huge green dead dragon is lying off to one side.")
(new-obj 'SHADOW_ #f 'SHADOW 'window)
(new-obj 'SHADOW #f 'SHADOW 'windoe
         "The shadowy figure seems to be trying to attract your attention.")
(new-obj 'PLANT2_ #f 'PLANT2 'e2pit)
(new-obj 'PLANT2 #f 'PLANT2 'w2pit
         #f
         "The top of a 12-foot-tall beanstalk is poking out of the west pit."
         "There is a huge beanstalk growing out of the west pit up to the hole.")
(new-obj 'CRYSTAL_ #f 'CRYSTAL 'wfiss)
(new-obj 'CRYSTAL #f 'CRYSTAL 'efiss
         #f
         "A crystal bridge now spans the fissure."
         "The crystal bridge has vanished!")
(new-obj 'TREADS_ #f 'TREADS 'emist)
(new-obj 'TREADS #f 'TREADS 'spit
         "Rough stone steps lead down the pit."
         "Rough stone steps lead up the dome.")
(new-obj 'GRATE_ #f 'GRATE 'inside)
(new-obj 'GRATE #f 'GRATE 'outside
         "The grate is locked."
         "The grate is open.")
(new-obj 'MIRROR_ #f 'MIRROR 'limbo)

;; one-place objects
(new-obj 'CHAIN "Golden chain" 'CHAIN 'barr
         "There is a golden chain lying in a heap on the floor!"
         "The bear is locked to the wall with a golden chain!"
         "There is a golden chain locked to the wall!")
(new-obj 'SPICES "Rare spices" #f 'chamber
         "There are rare spices here!")
(new-obj 'PEARL "Glistening pearl" #f 'limbo
         "Off to one side lies a glistening pearl!")
(new-obj 'PYRAMID "Platinum pyramid" #f 'droom
         "There is a platinum pyramid here, 8 inches on a side!")
(new-obj 'EMERALD "Egg-sized emerald" #f 'proom
         "There is an emerald here the size of a plover's egg!")
(new-obj 'VASE "Ming vase" #f 'oriental
         "There is a delicate, precious, Ming vase here!"
         "The vase is now resting, delicately, on a velvet pillow."
         "The floor is littered with worthless shards of pottery."
         "The Ming vase drops with a delicate crash.")
(new-obj 'TRIDENT "Jeweled trident" #f 'falls
         "There is a jewel-encrusted trident here!")
(new-obj 'EGGS "Golden eggs" #f 'giant
         "There is a large nest here, full of golden eggs!"
         "The nest of golden eggs has vanished!"
         "Done!")
(new-obj 'CHEST "Treasure chest" #f 'limbo
         "The pirate's treasure chest is here!")
(new-obj 'COINS "Rare coins" #f 'west
         "There are many coins here!")
(new-obj 'JEWELS "Precious jewelry" #f 'south
         "There is precious jewelry here!")
(new-obj 'SILVER "Bars of silver" #f 'ns
         "There are bars of silver here!")
(new-obj 'DIAMONDS "Several diamonds" #f 'wfiss
         "There are diamonds here!")
(new-obj 'GOLD "Large gold nugget" #f 'nugget
         "There is a large sparkling nugget of gold here!")
(new-obj 'MOSS #f 'MOSS 'soft
         #f)
(new-obj 'BATTERIES "Batteries" #f 'limbo
         "There are fresh batteries here."
         "Some worn-out batteries have been discarded nearby.")
(new-obj 'PONY #f 'PONY 'pony
         "There is a massive vending machine here.  The instructions on it read:\n\
\"Drop coins here to receive fresh batteries.\"")
(new-obj 'GEYSER #f 'GEYSER 'view
         #f)
(new-obj 'MESSAGE #f 'MESSAGE 'limbo
         "There is a message scrawled in the dust in a flowery script, reading:\n\
\"This is not the maze where the pirate hides his treasure chest.\"")
(new-obj 'BEAR #f 'BEAR 'barr
         "There is a ferocious cave bear eying you from the far end of the room!"
         "There is a gentle cave bear sitting placidly in one corner."
         "There is a contented-looking bear wandering about nearby."
         #f)
(new-obj 'PIRATE #f 'PIRATE 'limbo
         #f)
(new-obj 'ART #f 'ART 'oriental
         #f)
(new-obj 'AXE "Dwarf's axe" #f 'limbo
         "There is a little axe here."
         "There is a little axe lying beside the bear.")
(new-obj 'STALACTITE #f 'STALACTITE 'tite
         #f)
(new-obj 'PLANT #f 'PLANT 'wpit
         "There is a tiny little plant in the pit, murmuring \"Water, water, ...\""
         "The plant spurts into furious growth for a few seconds."
         "There is a 12-foot-tall beanstalk stretching up out of the pit,\n\
bellowing \"Water!!  Water!!\""
         "The plant grows explosively, almost filling the bottom of the pit."
         "There is a gigantic beanstalk stretching all the way up to the hole."
         "You've over-watered the plant!  It's shriveling up!  It's, it's...")
(new-obj 'MIRROR #f 'MIRROR 'mirror
         #f)
(new-obj 'OIL "Oil in the bottle" #f 'limbo)
(new-obj 'WATER "Water in the bottle" #f 'limbo)
(new-obj 'BOTTLE "Small bottle" #f 'house
         "There is a bottle of water here."
         "There is an empty bottle here."
         "There is a bottle of oil here.")
(new-obj 'FOOD "Tasty food" #f 'house
         "There is food here.")
(new-obj 'KNIFE #f #f 'limbo)
(new-obj 'DWARF #f 'DWARF 'limbo)
(new-obj 'MAG "\"Spelunker Today\"" #f 'ante
         "There are a few recent issues of \"Spelunker Today\" magazine here.")
(new-obj 'OYSTER "Giant oyster >GROAN!<" #f 'limbo
         "There is an enormous oyster here with its shell tightly closed."
         "Interesting.  There seems to be something written \
on the underside of\nthe oyster.")
(new-obj 'CLAM "Giant clam >GRUNT!<" #f 'shell
         "There is an enormous clam here with its shell tightly closed.")
(new-obj 'TABLET #f 'TABLET 'droom
         "A massive stone tablet embedded in the wall reads:\n\
\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\"")
(new-obj 'SNAKE #f 'SNAKE 'hmk
         "A huge green fierce snake bars the way!"
         #f)
(new-obj 'PILLOW "Velvet pillow" #f 'soft
         "A small velvet pillow lies on the floor.")
(new-obj 'DOOR #f 'DOOR 'immense
         "The way north is barred by a massive, rusty, iron door."
         "The way north leads through a massive, rusty, iron door.")
(new-obj 'BIRD "Little bird in cage" #f 'bird
         "A cheerful little bird is sitting here singing."
         "There is a little bird in the cage.")
(new-obj 'ROD2 "Black rod" #f 'limbo
         "A three-foot black rod with a rusty mark on an end lies nearby.")
(new-obj 'ROD "Black rod" #f 'debris
         "A three-foot black rod with a rusty star on an end lies nearby.")
(new-obj 'CAGE "Wicker cage" #f 'cobbles
         "There is a small wicker cage discarded nearby.")
(new-obj 'LAMP "Brass lantern" #f 'house
         "There is a shiny brass lamp nearby."
         "There is a lamp shining nearby.")
(new-obj 'KEYS "Set of keys" #f 'house
         "There are some keys on the ground here.")

(add-unl-macro!
 'initial-base '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (churchnum x)))
               (vector->list object-base))))

(add-unl-macro!
 'initial-prop '()
 `(list ,@(map (lambda (x)
                 (if (or (undefined? x) (< x 0))
                     'V
                     (churchnum x)))
               (vector->list object-prop))))

(add-unl-macro!
 'initial-place '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (churchnum x)))
               (vector->list object-place))))

(add-unl-macro!
 'objname '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list object-name))))

(add-unl-macro!
 'initial-note '()
 (compile-to-file
  "note.unlo"
  `(list ,@(map (lambda (lst)
		  (if (undefined? lst)
		      'V
		      (cons 'list
			    (map (lambda (x) (if x (list 'string x) 'V)) lst))))
		(vector->list object-note)))))

(defmacro (toting? object world)
  (not ((nth object (place world)) I I)))

(defmacro (at-loc? object world)
  (= (nth object (place world)) (location world)))

(defmacro (objects-here world)
  (let ((loc (location world)))
    ((lambda (x) (x x))
     (lambda (rec lst n)
       (lst
        (lambda (hd tl)
          ((if (= hd loc) (cons n) I)
           (rec rec tl (succ n))))))
     (place world)
     c0)))

(defmacro (objects-toting world)
  (let loop ((lst (cdr (place world)))
             (n c1))
    (lst
     (lambda (hd tl)
       ((if (hd I I) I (cons n))
        (loop tl (succ n)))))))

(defmacro (carry object world)
  (set-place
   world
   (modify-nth (to-cons1 object)
               (K V))))

(defmacro (drop object location world)
  (set-place world
             (modify-nth (to-cons1 object)
                         (K location))))
