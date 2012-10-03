#!/usr/bin/env gosh
(use file.util)
(require "enum.scm")

(define parser-trie (make-tree-map char=? char<?))

(define (trie-put! trie s k value)
  (cond ((<= (string-length s) k)
         (tree-map-put! trie #\null value))
        ((= k 4)
         (tree-map-put! trie (string-ref s k) value))
        (else
         (tree-map-update!
          trie
          (string-ref s k)
          (lambda (t)
            (let ((t (or t (make-tree-map char=? char<?))))
              (trie-put! t s (+ k 1) value)
              t))
          #f))))

(define-enum
  '(motion-type
    object-type
    action-type
    message-type
    special-type))

(define (define-word type meaning . rest)
  (let ((m (if (number? meaning) meaning (lookup-enum meaning)))
        (aux (if (string? (car rest)) 'V (car rest)))
        (words (if (string? (car rest)) rest (cdr rest))))
    (for-each
     (lambda (w)
       (trie-put! parser-trie w 0 (compile-word (lookup-enum type) m aux)))
     words)))

(define (compile-word type meaning aux)
  `(lambda (f)
     (f ,(churchnum type)
        ,(churchnum meaning)
        ,aux)))

;; motion vocabulary
(define-word 'motion-type 'N "n" "north")
(define-word 'motion-type 'SOUTH "s" "south")
(define-word 'motion-type 'E "e" "east")
(define-word 'motion-type 'W "w")
(define-word 'motion-type 'W 'I "west")
(define-word 'motion-type 'NE "ne")
(define-word 'motion-type 'SE "se")
(define-word 'motion-type 'NW "nw")
(define-word 'motion-type 'SW "sw")
(define-word 'motion-type 'U "upwar" "up" "u" "above" "ascen")
(define-word 'motion-type 'D "downw" "down" "d" "desce")
(define-word 'motion-type 'L "left")
(define-word 'motion-type 'R "right")
(define-word 'motion-type 'IN "inwar" "insid" "in")
(define-word 'motion-type 'OUT "out" "outsi" "exit" "leave")
(define-word 'motion-type 'FORWARD "forwa" "conti" "onwar")
(define-word 'motion-type 'BACK "back" "retur" "retre")
(define-word 'motion-type 'OVER "over")
(define-word 'motion-type 'ACROSS "acros")
(define-word 'motion-type 'UPSTREAM "upstr")
(define-word 'motion-type 'DOWNSTREAM "downs")
(define-word 'motion-type 'ENTER "enter")
(define-word 'motion-type 'CRAWL "crawl")
(define-word 'motion-type 'JUMP "jump")
(define-word 'motion-type 'CLIMB "climb")
(define-word 'motion-type 'LOOK "look" "exami" "touch" "descr" "l")
(define-word 'motion-type 'CROSS "cross")
(define-word 'motion-type 'ROAD "road" "hill")
(define-word 'motion-type 'WOODS "fores")
(define-word 'motion-type 'VALLEY "valle")
(define-word 'motion-type 'HOUSE "build" "house")
(define-word 'motion-type 'GULLY "gully")
(define-word 'motion-type 'STREAM "strea")
(define-word 'motion-type 'DEPRESSION "depre")
(define-word 'motion-type 'ENTRANCE "entra")
(define-word 'motion-type 'CAVE "cave")
(define-word 'motion-type 'ROCK "rock")
(define-word 'motion-type 'SLAB "slab" "slabr")
(define-word 'motion-type 'BED "bed")
(define-word 'motion-type 'PASSAGE "passa" "tunne")
(define-word 'motion-type 'CAVERN "caver")
(define-word 'motion-type 'CANYON "canyo")
(define-word 'motion-type 'AWKWARD "awkwa")
(define-word 'motion-type 'SECRET "secre")
(define-word 'motion-type 'BEDQUILT "bedqu")
(define-word 'motion-type 'RESERVOIR "reser")
(define-word 'motion-type 'GIANT "giant")
(define-word 'motion-type 'ORIENTAL "orien")
(define-word 'motion-type 'SHELL "shell")
(define-word 'motion-type 'BARREN "barre")
(define-word 'motion-type 'BROKEN "broke")
(define-word 'motion-type 'DEBRIS "debri")
(define-word 'motion-type 'VIEW "view")
(define-word 'motion-type 'FORK "fork")
(define-word 'motion-type 'PIT "pit")
(define-word 'motion-type 'SLIT "slit")
(define-word 'motion-type 'CRACK "crack")
(define-word 'motion-type 'DOME "dome")
(define-word 'motion-type 'HOLE "hole")
(define-word 'motion-type 'WALL "wall")
(define-word 'motion-type 'HALL "hall")
(define-word 'motion-type 'ROOM "room")
(define-word 'motion-type 'FLOOR "floor")
(define-word 'motion-type 'STAIRS "stair")
(define-word 'motion-type 'STEPS "steps")
(define-word 'motion-type 'COBBLES "cobbl")
(define-word 'motion-type 'SURFACE "surfa")
(define-word 'motion-type 'DARK "dark")
(define-word 'motion-type 'LOW "low")
(define-word 'motion-type 'OUTDOORS "outdo")
(define-word 'motion-type 'Y2 "y2")
(define-word 'motion-type 'XYZZY "xyzzy")
(define-word 'motion-type 'PLUGH "plugh")
(define-word 'motion-type 'PLOVER "plove")
(define-word 'motion-type 'OFFICE "main" "offic")
(define-word 'motion-type 'NOWHERE "null" "nowhe")

;; object vocabulary
(define-word 'object-type 'KEYS "key" "keys")
(define-word 'object-type 'LAMP "lamp" "lante" "headl")
(define-word 'object-type 'GRATE "grate")
(define-word 'object-type 'CAGE "cage")
(define-word 'object-type 'ROD "rod")
(define-word 'object-type 'BIRD "bird")
(define-word 'object-type 'DOOR "door")
(define-word 'object-type 'PILLOW "velve" "pillo")
(define-word 'object-type 'SNAKE "snake")
(define-word 'object-type 'CRYSTAL "fissu")
(define-word 'object-type 'TABLET "table")
(define-word 'object-type 'CLAM "clam")
(define-word 'object-type 'OYSTER "oyste")
(define-word 'object-type 'MAG "magaz" "issue" "spelu" "\"spel")
(define-word 'object-type 'DWARF "dwarf" "dwarv")
(define-word 'object-type 'KNIFE "knife" "knive")
(define-word 'object-type 'FOOD "food" "ratio")
(define-word 'object-type 'BOTTLE "bottl" "jar")
(define-word 'object-type 'WATER "water" "h2o")
(define-word 'object-type 'OIL "oil")
(define-word 'object-type 'MIRROR "mirro")
(define-word 'object-type 'PLANT "plant" "beans")
(define-word 'object-type 'STALACTITE "stala")
(define-word 'object-type 'SHADOW "shado" "figur")
(define-word 'object-type 'AXE "axe")
(define-word 'object-type 'ART "drawi")
(define-word 'object-type 'PIRATE "pirat")
(define-word 'object-type 'DRAGON "drago")
(define-word 'object-type 'BRIDGE "chasm")
(define-word 'object-type 'TROLL "troll")
(define-word 'object-type 'BEAR "bear")
(define-word 'object-type 'MESSAGE "messa")
(define-word 'object-type 'GEYSER "volca" "geyse")
(define-word 'object-type 'PONY "vendi" "machi")
(define-word 'object-type 'BATTERIES "batte")
(define-word 'object-type 'MOSS "moss" "carpe")
(define-word 'object-type 'GOLD "gold" "nugge")
(define-word 'object-type 'DIAMONDS "diamo")
(define-word 'object-type 'SILVER "silve" "bars")
(define-word 'object-type 'JEWELS "jewel")
(define-word 'object-type 'COINS "coins")
(define-word 'object-type 'CHEST "chest" "box" "treas")
(define-word 'object-type 'EGGS "eggs" "egg" "nest")
(define-word 'object-type 'TRIDENT "tride")
(define-word 'object-type 'VASE "ming" "vase" "shard" "potte")
(define-word 'object-type 'EMERALD "emera")
(define-word 'object-type 'PYRAMID "plati" "pyram")
(define-word 'object-type 'PEARL "pearl")
(define-word 'object-type 'RUG "persi" "rug")
(define-word 'object-type 'SPICES "spice")
(define-word 'object-type 'CHAIN "chain")

;; action vocabulary
(define-word 'action-type 'TAKE "take" "carry" "keep" "catch" "captu" "steal" "get" "tote")
(define-word 'action-type 'DROP "drop" "relea" "free" "disca" "dump")
(define-word 'action-type 'OPEN "open" "unloc")
(define-word 'action-type 'CLOSE "close" "lock")
(define-word 'action-type 'ON "light" "on")
(define-word 'action-type 'OFF "extin" "off")
(define-word 'action-type 'WAVE "wave" "shake" "swing")
(define-word 'action-type 'CALM "calm" "placa" "tame")
(define-word 'action-type 'GO "walk" "run" "trave" "go" "proce" "explo" "goto" "follo" "turn")
(define-word 'action-type 'RELAX "nothi")
(define-word 'action-type 'POUR "pour")
(define-word 'action-type 'EAT "eat" "devou")
(define-word 'action-type 'DRINK "drink")
(define-word 'action-type 'RUB "rub")
(define-word 'action-type 'TOSS "throw" "toss")
(define-word 'action-type 'WAKE "wake" "distu")
(define-word 'action-type 'FEED "feed")
(define-word 'action-type 'FILL "fill")
(define-word 'action-type 'BREAK "break" "smash" "shatt")
(define-word 'action-type 'BLAST "blast" "deton" "ignit" "blowu")
(define-word 'action-type 'KILL "attac" "kill" "fight" "hit" "strik" "slay")
(define-word 'action-type 'SAY "say" "chant" "sing" "utter" "mumbl")
(define-word 'action-type 'READ "read" "perus")
(define-word 'action-type 'FEEFIE 'c0 "fee")
(define-word 'action-type 'FEEFIE 'c1 "fie")
(define-word 'action-type 'FEEFIE 'c2 "foe")
(define-word 'action-type 'FEEFIE 'c3 "foo")
(define-word 'action-type 'FEEFIE 'c4 "fum")
(define-word 'action-type 'BRIEF "brief")
(define-word 'action-type 'FIND "find" "where")
(define-word 'action-type 'INVENTORY "inven" "i")
(define-word 'action-type 'SCORE "score")
(define-word 'action-type 'QUIT "quit")

(define default-msg (make-vector 30 #f))

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
The water tastes strongly of
minerals, but is not unpleasant.  It is extremely cold.")
(set-default-msg 'RUB "Rubbing the electric lamp \
is not particularly rewarding.  Anyway,
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
as you move about and manipulate
things.  I cannot tell you where remote things are.")
(set-default-msg 'INVENTORY "I can only tell you what you see \
as you move about and manipulate
things.  I cannot tell you where remote things are.")
(set-default-msg 'SCORE "Eh?")
(set-default-msg 'QUIT "Eh?")

(add-unl-macro!
 'default-msg '()
 (compress-list (map (lambda (x) (if x (list 'string x) 'V))
		     default-msg)))

(define messages '())

(define (define-message-word words message)
  (apply define-word (cons* 'message-type (length messages) words))
  (push! messages message))

(define-message-word
  '("abra" "abrac" "opens" "sesam" "shaza" "hocus" "pocus")
  "Good try, but that is an old worn-out magic word.")

(define-message-word
  '("?" "help")
  "I know of places, actions, and things.  Most of my vocabulary
describes places and is used to move you there.  To move, try words
like forest, building, downstream, enter, east, west, north, south,
up, or down.  I know about a few special objects, like a black rod
hidden in the cave.  These objects can be manipulated using some of
the action words that I know.  Usually you will need to give both the
object and action words (in either order), but sometimes I can infer
the object from the verb alone.  Some objects also imply verbs; in
particular, \"inventory\" implies \"take inventory\", which causes me to
give you a list of what you're carrying.  The objects have side
effects; for instance, the rod scares the bird.  Usually people having
trouble moving just need to try a few more words.  Usually people
trying unsuccessfully to manipulate an object are attempting something
beyond their (or my!) capabilities and should try a completely
different tack.  To speed the game you can sometimes move long
distances with a single word.  For example, \"building\" usually gets
you to the building from anywhere above ground except when lost in the
forest.  Also, note that cave passages turn a lot, and that leaving a
room to the north does not guarantee entering the next from the south.
Good luck!")

(define-message-word
  '("tree" "trees")
  "The trees of the forest are large hardwood oak and maple, with an
occasional grove of pine or spruce.  There is quite a bit of under-
growth, largely birch and ash saplings plus nondescript bushes of
various sorts.  This time of year visibility is quite restricted by
all the leaves, but travel is quite easy if you detour around the
spruce and berry bushes.")

(define-message-word
  '("dig" "excav")
  "Digging without a shovel is quite impractical.  Even with a shovel
progress is unlikely.")

(define-message-word
  '("lost")
  "I'm as confused as you are.")

(define-message-word
  '("mist")
  "Mist is a white vapor, usually water, seen from time to time in
caverns.  It can be found anywhere but is frequently a sign of a deep
pit leading down to water.")

(define-message-word
  '("fuck")
  "Watch it!")

(define-message-word
  '("stop")
  "I don't know the word \"stop\".  Use \"quit\" if you want to give up.")

(define-message-word
  '("info" "infor")
  "If you want to end your adventure early, say \"quit\".  To get full
credit for a treasure, you must have left it safely in the building,
though you get partial credit just for locating it.  You lose points
for getting killed, or for quitting, though the former costs you more.
There are also points based on how much (if any) of the cave you've
managed to explore; in particular, there is a large bonus just for
getting in (to distinguish the beginners from the rest of the pack),
and there are other ways to determine whether you've been through some
of the more harrowing sections.  If you think you've found all the
treasures, just keep exploring for a while.  If nothing interesting
happens, you haven't found them all yet.  If something interesting
DOES happen, it means you're getting a bonus and have an opportunity
to garner many more points in the master's section.
I may occasionally offer hints if you seem to be having trouble.
If I do, I'll warn you in advance how much it will affect your score
to accept the hints.  Finally, to save paper, you may specify \"brief\",
which tells me never to repeat the full description of a place
unless you explicitly ask me to.")

(define-message-word
  '("swim")
  "I don't know how.")

(add-unl-macro!
 'message '()
 (compress-list (map (lambda (x) (list 'string x))
		     (reverse messages))))

(define-word 'special-type 0 "y" "yes")

(defmacro (reader-core cont)
  ((lambda-rec rec (p b)
     (if b
         (let ((next (rec (! (S p)))))
           (K (lambda (f) (f next))
              (@ I)))
         (lambda (word)
           (let loop ((p p))
             (if (read-char=? #\space #\newline)
                 (cont (cons p word))
                 (loop (K (! (S p))
                          (@ I))))))))
   I))

(define (generate-parser-rec trie)
  `(lambda (reader)
     (,@(tree-map-map
         trie
         (lambda (ch t)
           (if (eq? ch #\null)
               `((read-char=? #\space #\newline) reader V ,t)
               `(,(if (char-alphabetic? ch)
                      `(read-char=? ,(char-downcase ch) ,(char-upcase ch))
                      `(read-char=? ,ch))
                 ,@(if (is-a? t <tree-map>)
                       `(reader I ,(generate-parser-rec t))
                       `(reader V ,t))))))
      (reader V V))))

(define (generate-parser)
  (generate-parser-rec parser-trie))

(add-unl-macro!
 'generated-parser '()
 (compile-to-file
  "parser.unlo"
  `(lambda (cont)
     (,(generate-parser) (reader-core cont)))))

;;; parser macros

(defmacro (word? word) ((cdr word) (lambda (_ _ _) I)))
(defmacro (word-type word) ((cdr word) (lambda (t _ _) t)))
(defmacro (motion? word) (zero? (word-type word)))
(defmacro (noun? word) (= object-type (word-type word)))
(defmacro (verb? word) (= action-type (word-type word)))
(defmacro (message-word? word) (= message-type (word-type word)))
(defmacro (special-word? word) (= special-type (word-type word)))
(defmacro (word-meaning word) ((cdr word) (lambda (_ m _) m)))
(defmacro (word-aux word) ((cdr word) (lambda (_ _ a) a)))
(defmacro (word-letters word) (car word))

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
  (let ((parser generated-parser))
    (call/cc
     (lambda (return)
       ((@ I)
        skip-spaces
        ((?newline I) (string " Tell me to do something.\n") return V)  ; empty input
        (let ((word1 (call/cc parser)))
          (skip-spaces
           ((?newline I) return (icons word1 V))
           (let ((word2 (call/cc parser)))
             (skip-spaces
              ((?newline I) return (icons word1 word2))
              (skip-until-newline)
              (return (print-and-return "Please stick to 1- and 2-word commands.\n" V)))))))))))

(defmacro listen
  (call/cc
   (lambda (return)
     (lambda-rec rec ()
       ((print "* ")
        (let ((words getin))
          (((pair? words) return words)
           (rec))))))))

(defmacro (make-word type meaning str aux)
  (cons str (lambda (f) (f type meaning aux))))
(defmacro (make-non-word str)
  (cons str V))
(defmacro dummy-word
  (make-word c1 c1 I V))
(defmacro (motion-word meaning)
  (make-word motion-type meaning I V))
(defmacro (object-word meaning)
  (make-word object-type meaning I V))
(defmacro (action-word meaning)
  (make-word action-type meaning I V))
(defmacro (message-word meaning)
  (make-word message-type meaning I V))
(defmacro (motion-word-with-aux meaning aux)
  (make-word motion-type meaning I aux))
(defmacro (action-word-with-aux meaning aux)
  (make-word action-type meaning I aux))
(defmacro (object-word-with-str meaning str)
  (make-word object-type meaning str V))
(defmacro (action-word-with-str meaning str)
  (make-word action-type meaning str V))

(defmacro (yes q y n)
  (call/cc
   (lambda (ret)
     ((call/cc I)
      (call/cc
       (let ((skip (delay (K I skip-until-newline))))
	 (begin
	   (q (string "\n** ") @ I)
	   ((read-char=? #\y #\Y) skip y #\newline ret I)
	   ((read-char=? #\n #\N) skip n #\newline ret V)
	   (skip
	    (print " Please answer Yes or No.\n")))))))))
