#!/usr/bin/env gosh
(use file.util)
(require "enum.scm")

(define ignore-case #f)

(define parser-trie (make-tree-map char=? char<?))

(define (trie-put! trie chars value)
  (if (null? chars)
      (tree-map-put! trie #\null value)
      (tree-map-update!
       trie
       (car chars)
       (lambda (t)
         (let ((t (or t (make-tree-map char=? char<?))))
           (trie-put! t (cdr chars) value)
           t))
       #f)))

(define motion-type 0)
(define object-type 1)
(define action-type 2)
(define message-type 3)

(define (define-word type meaning . words)
  (let ((m (if (number? meaning) meaning (lookup-enum meaning))))
    (for-each
     (lambda (w)
       (trie-put! parser-trie (string->list w) (list type m w)))
     words)))

;; motion vocabulary
(define-word motion-type 'N "n" "north")
(define-word motion-type 'SOUTH "s" "south")
(define-word motion-type 'E "e" "east")
(define-word motion-type 'W "w" "west")
(define-word motion-type 'NE "ne" "northeast")
(define-word motion-type 'SE "se" "southeast")
(define-word motion-type 'NW "nw" "northwest")
(define-word motion-type 'SW "sw" "southwest")
(define-word motion-type 'U "upward" "up" "u" "above" "ascend")
(define-word motion-type 'D "downward" "down" "d" "descend")
(define-word motion-type 'L "left")
(define-word motion-type 'R "right")
(define-word motion-type 'IN "inward" "inside" "in")
(define-word motion-type 'OUT "out" "outside" "exit" "leave")
(define-word motion-type 'FORWARD "forward" "onward")  ; TODO: add "continue"?
(define-word motion-type 'BACK "back" "return" "retreat")
(define-word motion-type 'OVER "over")
(define-word motion-type 'ACROSS "across")
(define-word motion-type 'UPSTREAM "upstream")
(define-word motion-type 'DOWNSTREAM "downstream")
(define-word motion-type 'ENTER "enter")
(define-word motion-type 'CRAWL "crawl")
(define-word motion-type 'JUMP "jump")
(define-word motion-type 'CLIMB "climb")
(define-word motion-type 'LOOK "look" "examine" "touch" "describe" "l")
(define-word motion-type 'CROSS "cross")
(define-word motion-type 'ROAD "road" "hill")
(define-word motion-type 'WOODS "forest")
(define-word motion-type 'VALLEY "valley")
(define-word motion-type 'HOUSE "building" "house")
(define-word motion-type 'GULLY "gully")
(define-word motion-type 'STREAM "stream")
(define-word motion-type 'DEPRESSION "depression")
(define-word motion-type 'ENTRANCE "entrance")
(define-word motion-type 'CAVE "cave")
(define-word motion-type 'ROCK "rock")
(define-word motion-type 'SLAB "slab")  ; TODO: "slabr"?
(define-word motion-type 'BED "bed")
(define-word motion-type 'PASSAGE "passage" "tunnel")
(define-word motion-type 'CAVERN "cavern")
(define-word motion-type 'CANYON "canyon")
(define-word motion-type 'AWKWARD "awkward")
(define-word motion-type 'SECRET "secret")
(define-word motion-type 'BEDQUILT "bedquilt")
(define-word motion-type 'RESERVOIR "reservoir")
(define-word motion-type 'GIANT "giant")
(define-word motion-type 'ORIENTAL "oriental")
(define-word motion-type 'SHELL "shell")
(define-word motion-type 'BARREN "barren")
(define-word motion-type 'BROKEN "broken")
(define-word motion-type 'DEBRIS "debris")
(define-word motion-type 'VIEW "view")
(define-word motion-type 'FORK "fork")
(define-word motion-type 'PIT "pit")
(define-word motion-type 'SLIT "slit")
(define-word motion-type 'CRACK "crack")
(define-word motion-type 'DOME "dome")
(define-word motion-type 'HOLE "hole")
(define-word motion-type 'WALL "wall")
(define-word motion-type 'HALL "hall")
(define-word motion-type 'ROOM "room")
(define-word motion-type 'FLOOR "floor")
(define-word motion-type 'STAIRS "stairs")
(define-word motion-type 'STEPS "steps")
(define-word motion-type 'COBBLES "cobbles" "cobblestone" "cobblestones")  ; TODO: "cobble" too?
(define-word motion-type 'SURFACE "surface")
(define-word motion-type 'DARK "dark")
(define-word motion-type 'LOW "low")
(define-word motion-type 'OUTDOORS "outdoors")
(define-word motion-type 'Y2 "y2")
(define-word motion-type 'XYZZY "xyzzy")
(define-word motion-type 'PLUGH "plugh")
(define-word motion-type 'PLOVER "plover")
(define-word motion-type 'OFFICE "main" "office")
(define-word motion-type 'NOWHERE "nowhere" "null")

;; object vocabulary
(define-word object-type 'KEYS "key" "keys")
(define-word object-type 'LAMP "lamp" "lantern" "headlamp")
(define-word object-type 'GRATE "grate")
(define-word object-type 'CAGE "cage")
(define-word object-type 'ROD "rod")
(define-word object-type 'BIRD "bird")
(define-word object-type 'DOOR "door")
(define-word object-type 'PILLOW "velvet" "pillow")
(define-word object-type 'SNAKE "snake")
(define-word object-type 'CRYSTAL "fissure")
(define-word object-type 'TABLET "tablet")
(define-word object-type 'CLAM "clam")
(define-word object-type 'OYSTER "oyster")
(define-word object-type 'MAG "magazine" "issue" "spelunker" "\"spelunker today\"")
(define-word object-type 'DWARF "dwarf" "dwarves")
(define-word object-type 'KNIFE "knife" "knives")
(define-word object-type 'FOOD "food" "rations")
(define-word object-type 'BOTTLE "bottle" "jar")
(define-word object-type 'WATER "water" "h2o")
(define-word object-type 'OIL "oil")
(define-word object-type 'MIRROR "mirror")
(define-word object-type 'PLANT "plant" "beans")
(define-word object-type 'STALACTITE "stalactite")
(define-word object-type 'SHADOW "shadow" "figure")
(define-word object-type 'AXE "axe")
(define-word object-type 'ART "drawing")
(define-word object-type 'PIRATE "pirate")
(define-word object-type 'DRAGON "dragon")
(define-word object-type 'BRIDGE "chasm")
(define-word object-type 'TROLL "troll")
(define-word object-type 'BEAR "bear")
(define-word object-type 'MESSAGE "message")
(define-word object-type 'GEYSER "volcano" "geyser")
(define-word object-type 'PONY "vending" "machine")
(define-word object-type 'BATTERIES "battery" "batteries")
(define-word object-type 'MOSS "moss" "carpet")
(define-word object-type 'GOLD "gold" "nugget")
(define-word object-type 'DIAMONDS "diamonds")
(define-word object-type 'SILVER "silver" "bars")
(define-word object-type 'JEWELS "jewel" "jewelry" "jewels")
(define-word object-type 'COINS "coins")
(define-word object-type 'CHEST "chest" "box" "treasure")
(define-word object-type 'EGGS "egg" "eggs" "nest")
(define-word object-type 'TRIDENT "trident")
(define-word object-type 'VASE "ming" "vase" "shard" "pottery")
(define-word object-type 'EMERALD "emerald")
(define-word object-type 'PYRAMID "platinum" "pyramid")
(define-word object-type 'PEARL "pearl")
(define-word object-type 'RUG "persian" "rug")
(define-word object-type 'SPICES "spice" "spices")
(define-word object-type 'CHAIN "chain")

;; action vocabulary
(define-word action-type 'TAKE "capture" "carry" "catch" "get" "keep" "steal" "take" "tote")
(define-word action-type 'DROP "discard" "drop" "dump" "free" "release")
(define-word action-type 'OPEN "open" "unlock")
(define-word action-type 'CLOSE "close" "lock")
(define-word action-type 'ON "light" "on")
(define-word action-type 'OFF "extinguish" "off")
(define-word action-type 'WAVE "shake" "swing" "wave")
(define-word action-type 'CALM "calm" "placate" "tame")
(define-word action-type 'GO "explore" "follow" "go" "goto" "proceed" "run" "travel" "turn" "walk")  ; TODO: "continue" too?
(define-word action-type 'RELAX "nothing")
(define-word action-type 'POUR "pour")
(define-word action-type 'EAT "devour" "eat")
(define-word action-type 'DRINK "drink")
(define-word action-type 'RUB "rub")
(define-word action-type 'TOSS "throw" "toss")
(define-word action-type 'WAKE "disturb" "wake")
(define-word action-type 'FEED "feed")
(define-word action-type 'FILL "fill")
(define-word action-type 'BREAK "break" "shatter" "smash")
(define-word action-type 'BLAST "blast" "blowup" "detonate" "ignite")
(define-word action-type 'KILL "attack" "fight" "hit" "kill" "strike" "slay")
(define-word action-type 'SAY "chant" "mumble" "say" "sing" "utter")
(define-word action-type 'READ "peruse" "read")
(define-word action-type 'FEEFIE "fee" "fie" "foe" "foo" "fum")
(define-word action-type 'BRIEF "brief")
(define-word action-type 'FIND "find" "where")
(define-word action-type 'INVENTORY "inventory" "i")
(define-word action-type 'SCORE "score")
(define-word action-type 'QUIT "quit")

(define default-msg (make-vector 30))

(define (set-default-msg verb msg)
  (vector-set! default-msg (lookup-enum verb) msg))

(set-default-msg 'TAKE "You are already carrying it!")
(set-default-msg 'DROP "You aren't carrying it!")
(set-default-msg 'OPEN "I don't know how to lock or unlock such a thing.")
(set-default-msg 'CLOSE "I don't know how to lock or unlock such a thing.")
(set-default-msg 'ON "You have no source of light.")
(set-default-msg 'OFF "You have no source of light.")
(set-default-msg 'WAVE "Nothing happens.")
(set-default-msg 'CALM "I'm game.  Would you care to explain how?")
(set-default-msg 'GO "Where?")
(set-default-msg 'RELAX "OK.")
(set-default-msg 'POUR "You aren't carrying it!")
(set-default-msg 'EAT "Don't be ridiculous!")
(set-default-msg 'DRINK "You have taken a drink from the stream.  \
The water tastes strongly of\n\
minerals, but is not unpleasant.  It is extremely cold.")
(set-default-msg 'RUB "Rubbing the electric lamp \
is not particularly rewarding.  Anyway,\n\
nothing exciting happens.")
(set-default-msg 'TOSS "Peculiar.  Nothing unexpected happens.")
(set-default-msg 'WAKE "Don't be ridiculous!")
(set-default-msg 'FEED "There is nothing here to eat.")
(set-default-msg 'FILL "You can't fill that.")
(set-default-msg 'BREAK "It is beyond your power to do that.")
(set-default-msg 'BLAST "Blasting requires dynamite.")
(set-default-msg 'KILL "Don't be ridiculous!")
(set-default-msg 'READ "I'm afraid I don't understand.")
(set-default-msg 'FEEFIE "I don't know how.")
(set-default-msg 'BRIEF "On what?")
(set-default-msg 'FIND "I can only tell you what you see \
as you move about and manipulate\n\
things.  I cannot tell you where remote things are.")
(set-default-msg 'INVENTORY "I can only tell you what you see \
as you move about and manipulate\n\
things.  I cannot tell you where remote things are.")
(set-default-msg 'SCORE "Eh?")
(set-default-msg 'QUIT "Eh?")

(add-unl-macro!
 'initial-default-msg '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list default-msg))))

(define messages '())

(define (define-message-word words message)
  (apply define-word (cons* message-type (length messages) words))
  (push! messages message))

(define-message-word
  '("abra" "abracadabra" "hocus" "opensesame" "pocus" "sesame" "shazam")
  "Good try, but that is an old worn-out magic word.")

(define-message-word
  '("?" "help")
  "I know of places, actions, and things.  Most of my vocabulary\n\
describes places and is used to move you there.  To move, try words\n\
like forest, building, downstream, enter, east, west, north, south,\n\
up, or down.  I know about a few special objects, like a black rod\n\
hidden in the cave.  These objects can be manipulated using some of\n\
the action words that I know.  Usually you will need to give both the\n\
object and action words (in either order), but sometimes I can infer\n\
the object from the verb alone.  Some objects also imply verbs; in\n\
particular, \"inventory\" implies \"take inventory\", which causes me to\n\
give you a list of what you're carrying.  The objects have side\n\
effects; for instance, the rod scares the bird.  Usually people having\n\
trouble moving just need to try a few more words.  Usually people\n\
trying unsuccessfully to manipulate an object are attempting something\n\
beyond their (or my!) capabilities and should try a completely\n\
different tack.  To speed the game you can sometimes move long\n\
distances with a single word.  For example, \"building\" usually gets\n\
you to the building from anywhere above ground except when lost in the\n\
forest.  Also, note that cave passages turn a lot, and that leaving a\n\
room to the north does not guarantee entering the next from the south.\n\
Good luck!")

(define-message-word
  '("tree" "trees")
  "The trees of the forest are large hardwood oak and maple, with an\n\
occasional grove of pine or spruce.  There is quite a bit of under-\n\
growth, largely birch and ash saplings plus nondescript bushes of\n\
various sorts.  This time of year visibility is quite restricted by\n\
all the leaves, but travel is quite easy if you detour around the\n\
spruce and berry bushes.")

(define-message-word
  '("dig" "excavate")
  "Digging without a shovel is quite impractical.  Even with a shovel\n\
progress is unlikely.")

(define-message-word
  '("lost")
  "I'm as confused as you are.")

(define-message-word
  '("mist")
  "Mist is a white vapor, usually water, seen from time to time in\n\
caverns.  It can be found anywhere but is frequently a sign of a deep\n\
pit leading down to water.")

(define-message-word
  '("fuck")
  "Watch it!")

(define-message-word
  '("stop")
  "I don't know the word \"stop\".  Use \"quit\" if \
you want to give up.")

(define-message-word
  '("info" "information")
  "If you want to end your adventure early, say \"quit\".  To get full\n\
credit for a treasure, you must have left it safely in the building,\n\
though you get partial credit just for locating it.  You lose points\n\
for getting killed, or for quitting, though the former costs you more.\n\
There are also points based on how much (if any) of the cave you've\n\
managed to explore; in particular, there is a large bonus just for\n\
getting in (to distinguish the beginners from the rest of the pack),\n\
and there are other ways to determine whether you've been through some\n\
of the more harrowing sections.  If you think you've found all the\n\
treasures, just keep exploring for a while.  If nothing interesting\n\
happens, you haven't found them all yet.  If something interesting\n\
DOES happen, it means you're getting a bonus and have an opportunity\n\
to garner many more points in the master's section.\n\
I may occasionally offer hints if you seem to be having trouble.\n\
If I do, I'll warn you in advance how much it will affect your score\n\
to accept the hints.  Finally, to save paper, you may specify \"brief\",\n\
which tells me never to repeat the full description of a place\n\
unless you explicitly ask me to.")

(define-message-word
  '("swim")
  "I don't know how.")

(add-unl-macro!
 'initial-message '()
 `(list ,@(map (lambda (x) (list 'string x))
               (reverse messages))))

(define (defword word-data)
  (let ((type (car word-data))
        (meaning (cadr word-data))
        (letters (caddr word-data)))
    `(lambda (f)
       (f (lambda (f0 f1 f2 f3)
            ,(string->symbol (string-append "f" (number->string type))))
          ,(churchnum meaning)
          (string ,letters)))))

(define (generate-parser-rec read trie)
  `(lambda (return)
     (,(if read '(@ I) 'I)
      ,@(tree-map-map
         trie
         (lambda (ch t)
           (if (eq? ch #\null)
               `((read-char=? #\space #\newline) return ,(defword t))
               `(,(if (and ignore-case (char-alphabetic? ch))
                      `(read-char=? ,(char-downcase ch) ,(char-upcase ch))
                      `(read-char=? ,ch))
                 ,(generate-parser-rec #t t)
                 return))))
      (return V))))

(define (generate-parser)
  (generate-parser-rec #f parser-trie))

(add-unl-macro!
 'generated-parser '()
 (compile-to-file "parser.unlo" (generate-parser)))

;;; parser macros

(defmacro (word? word) (word (lambda (_ _ _) I)))
(defmacro (word-type word) (word (lambda (t _ _) t)))
(defmacro (motion? word) ((word-type word) I V V V))
(defmacro (noun? word) ((word-type word) V I V V))
(defmacro (verb? word) ((word-type word) V V I V))
(defmacro (message-word? word) ((word-type word) V V V I))
(defmacro (word-meaning word) (word (lambda (_ m _) m)))
(defmacro (word-letters word) (word (lambda (_ _ l) l)))

(defrecmacro parse-lineend
  (lambda (q)
    (K parse-lineend
       (((?space I) q space-id)
        ((?newline I) q newline-id)))))

(defmacro skip-spaces
  ((call/cc I)
   (call/cc (?space I @ I))))

(defmacro skip-until-newline
  ((call/cc I)
   (call/cc ((call/cc
              (lambda (q)
                (K I (?newline I q V))))
             @ I))))


(defmacro getin
  ((lambda (parser)
     (call/cc
      (lambda (return)
        ((@ I)
         skip-spaces
         ((?newline I) (string " Tell me to do something.\n") return V)  ; empty input
         (let ((word1 (call/cc parser)))
           (if (word? word1)
               (skip-spaces
                ((?newline I) return (icons word1 V))
                (let ((word2 (call/cc parser)))
                  (if (word? word2)
                      (skip-spaces
                       ((?newline I) return (icons word1 word2))
                       (skip-until-newline)
                       (return (print$ "Please stick to 1- and 2-word commands.\n" V)))  ; more than 2 words
                    ((skip-until-newline) (return (icons V V))))))  ; unknown word
             ((skip-until-newline) (return (icons V V)))))))))  ; unknown word
   generated-parser))

(defmacro listen
  (call/cc
   (lambda (return)
     ((lambda (x) (x x))
      (lambda (rec)
        ((#\> I)
         (let ((words getin))
           (((words (lambda (hd tl) I)) return words)
            (rec rec)))))))))

(defmacro parser-main
  (call/cc
   (lambda (q)
     ((listen
      (lambda (w1 w2)
        ((print-digit (word-meaning w1) I)
         (#\space I)
         ((message-word? w1)
          (nth (word-meaning w1) initial-message) I
          #\space I)
         (if (word? w2)
             (print-digit (word-meaning w2) I)
           ((string "none") I))
         (q I))))
      ((string "what?") I)))))

(define (main args)
  (print-as-unl 'generated-parser)
  0)
