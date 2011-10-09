#!/usr/bin/env gosh

(define ignore-case #f)

(define words
  '(("spelunker today" . 1016)
    ("?" . 3051)
    ("above" . 29)
    ("abra" . 3050)
    ("abracadabra" . 3050)
    ("across" . 42)
    ("ascend" . 29)
    ("attack" . 2012)
    ("awkward" . 26)
    ("axe" . 1028)
    ("back" . 8)
    ("barren" . 40)
    ("bars" . 1052)
    ("batteries" . 1039)
    ("battery" . 1039)
    ("beans" . 1024)
    ("bear" . 1035)
    ("bed" . 16)
    ("bedquilt" . 70)
    ("bird" . 1008)
    ("blast" . 2023)
    ("blowup" . 2023)
    ("bottle" . 1020)
    ("box" . 1055)
    ("break" . 2028)
    ("brief" . 2026)
    ("broken" . 54)
    ("building" . 12)
    ("cage" . 1004)
    ("calm" . 2010)
    ("canyon" . 25)
    ("capture" . 2001)
    ("carpet" . 1040)
    ("carry" . 2001)
    ("catch" . 2001)
    ("cave" . 67)
    ("cavern" . 73)
    ("chain" . 1064)
    ("chant" . 2003)
    ("chasm" . 1032)
    ("chest" . 1055)
    ("clam" . 1014)
    ("climb" . 56)
    ("close" . 2006)
    ("cobblestone" . 18)
    ("coins" . 1054)
    ("continue" . 2011)
    ("crack" . 33)
    ("crawl" . 17)
    ("cross" . 69)
    ("d" . 30)
    ("dark" . 22)
    ("debris" . 51)
    ("depression" . 63)
    ("descend" . 30)
    ("describe" . 57)
    ("detonate" . 2023)
    ("devour" . 2014)
    ("diamonds" . 1051)
    ("dig" . 3066)
    ("discard" . 2002)
    ("disturb" . 2029)
    ("dome" . 35)
    ("door" . 1009)
    ("down" . 30)
    ("downstream" . 5)
    ("downward" . 30)
    ("dragon" . 1031)
    ("drawing" . 1029)
    ("drink" . 2015)
    ("drop" . 2002)
    ("dump" . 2002)
    ("dwarf" . 1017)
    ("dwarves" . 1017)
    ("e" . 43)
    ("east" . 43)
    ("eat" . 2014)
    ("egg" . 1056)
    ("eggs" . 1056)
    ("emerald" . 1059)
    ("enter" . 3)
    ("entrance" . 64)
    ("examine" . 57)
    ("excavate" . 3066)
    ("exit" . 11)
    ("explore" . 2011)
    ("extinguish" . 2008)
    ("fee" . 2025)
    ("fee" . 3001)
    ("feed" . 2021)
    ("fie" . 2025)
    ("fie" . 3002)
    ("fight" . 2012)
    ("figure" . 1027)
    ("fill" . 2022)
    ("find" . 2019)
    ("fissure" . 1012)
    ("floor" . 58)
    ("foe" . 2025)
    ("foe" . 3003)
    ("follow" . 2011)
    ("foo" . 2025)
    ("foo" . 3004)
    ("food" . 1019)
    ("forest" . 6)
    ("fork" . 77)
    ("forward" . 7)
    ("free" . 2002)
    ("fuck" . 3079)
    ("fum" . 2025)
    ("fum" . 3005)
    ("get" . 2001)
    ("geyser" . 1037)
    ("giant" . 27)
    ("go" . 2011)
    ("gold" . 1050)
    ("goto" . 2011)
    ("grate" . 1003)
    ("gully" . 13)
    ("h2o" . 1021)
    ("hall" . 38)
    ("headlamp" . 1002)
    ("help" . 3051)
    ("hill" . 2)
    ("hit" . 2012)
    ("hocus" . 3050)
    ("hole" . 52)
    ("hours" . 2031)
    ("house" . 12)
    ("i" . 2020)
    ("ignite" . 2023)
    ("in" . 19)
    ("info" . 3142)
    ("information" . 3142)
    ("inside" . 19)
    ("inventory" . 2020)
    ("inward" . 19)
    ("issue" . 1016)
    ("jar" . 1020)
    ("jewel" . 1053)
    ("jewelry" . 1053)
    ("jewels" . 1053)
    ("jump" . 39)
    ("keep" . 2001)
    ("key" . 1001)
    ("keys" . 1001)
    ("kill" . 2012)
    ("knife" . 1018)
    ("knives" . 1018)
    ("l" . 57)
    ("lamp" . 1002)
    ("lantern" . 1002)
    ("leave" . 11)
    ("left" . 36)
    ("light" . 2007)
    ("lock" . 2006)
    ("look" . 57)
    ("lost" . 3068)
    ("low" . 24)
    ("machine" . 1038)
    ("magazine" . 1016)
    ("main" . 76)
    ("message" . 1036)
    ("ming" . 1058)
    ("mirror" . 1023)
    ("mist" . 3069)
    ("moss" . 1040)
    ("mumble" . 2003)
    ("n" . 45)
    ("ne" . 47)
    ("nest" . 1056)
    ("north" . 45)
    ("nothing" . 2005)
    ("nowhere" . 21)
    ("nugget" . 1050)
    ("null" . 21)
    ("nw" . 50)
    ("off" . 2008)
    ("office" . 76)
    ("oil" . 1022)
    ("on" . 2007)
    ("onward" . 7)
    ("open" . 2004)
    ("opensesame" . 3050)
    ("oriental" . 72)
    ("out" . 11)
    ("outdoors" . 32)
    ("outside" . 11)
    ("over" . 41)
    ("oyster" . 1015)
    ("passage" . 23)
    ("pause" . 2030)
    ("pearl" . 1061)
    ("persian" . 1062)
    ("peruse" . 2027)
    ("pillow" . 1010)
    ("pirate" . 1030)
    ("pit" . 31)
    ("placate" . 2010)
    ("plant" . 1024)
    ("plant" . 1025)
    ("platinum" . 1060)
    ("plover" . 71)
    ("plugh" . 65)
    ("pocus" . 3050)
    ("pottery" . 1058)
    ("pour" . 2013)
    ("proceed" . 2011)
    ("pyramid" . 1060)
    ("quit" . 2018)
    ("rations" . 1019)
    ("read" . 2027)
    ("release" . 2002)
    ("reservoir" . 75)
    ("retreat" . 8)
    ("return" . 8)
    ("right" . 37)
    ("road" . 2)
    ("rock" . 15)
    ("rod" . 1005)
    ("rod" . 1006)
    ("room" . 59)
    ("rub" . 2016)
    ("rug" . 1062)
    ("run" . 2011)
    ("s" . 46)
    ("save" . 2030)
    ("say" . 2003)
    ("score" . 2024)
    ("se" . 48)
    ("secret" . 66)
    ("sesame" . 3050)
    ("shadow" . 1027)
    ("shake" . 2009)
    ("shard" . 1058)
    ("shatter" . 2028)
    ("shazam" . 3050)
    ("shell" . 74)
    ("silver" . 1052)
    ("sing" . 2003)
    ("slab" . 61)
    ("slit" . 60)
    ("smash" . 2028)
    ("snake" . 1011)
    ("south" . 46)
    ("spelunker" . 1016)
    ("spice" . 1063)
    ("spices" . 1063)
    ("stairs" . 10)
    ("stalactite" . 1026)
    ("steal" . 2001)
    ("steps" . 1007)
    ("steps" . 34)
    ("stop" . 3139)
    ("stream" . 14)
    ("strike" . 2012)
    ("surface" . 20)
    ("suspend" . 2030)
    ("sw" . 49)
    ("swim" . 3147)
    ("swing" . 2009)
    ("tablet" . 1013)
    ("take" . 2001)
    ("tame" . 2010)
    ("throw" . 2017)
    ("toss" . 2017)
    ("tote" . 2001)
    ("touch" . 57)
    ("travel" . 2011)
    ("treasure" . 1055)
    ("tree" . 3064)
    ("trees" . 3064)
    ("trident" . 1057)
    ("troll" . 1033)
    ("troll" . 1034)
    ("tunnel" . 23)
    ("turn" . 2011)
    ("u" . 29)
    ("unlock" . 2004)
    ("up" . 29)
    ("upstream" . 4)
    ("upward" . 29)
    ("utter" . 2003)
    ("valley" . 9)
    ("vase" . 1058)
    ("velvet" . 1010)
    ("vending" . 1038)
    ("view" . 28)
    ("volcano" . 1037)
    ("w" . 44)
    ("wake" . 2029)
    ("walk" . 2011)
    ("wall" . 53)
    ("water" . 1021)
    ("wave" . 2009)
    ("west" . 44)
    ("where" . 2019)
    ("xyzzy" . 62)
    ("y2" . 55)
    ))

(define test-words
  '(("nw" . 10)
    ("ne" . 11)))

(define (make-trie) (make-hash-table))
(define (trie-put! trie chars value)
  (if (null? chars)
      (hash-table-put! trie 'eow value)
      (hash-table-update!
       trie
       (car chars)
       (lambda (t)
         (let ((t (or t (make-hash-table))))
           (trie-put! t (cdr chars) value)
           t))
       #f)))

(define (generate-parser-rec read trie)
  `(lambda (return)
     (,(if read '(@ I) 'I)
      ,@(hash-table-map
         trie
         (lambda (ch t)
           (if (eq? ch 'eow)
               `((read-char=? #\space #\newline) return (defword ,t))
               `(,(if (and ignore-case (char-alphabetic? ch))
                      `(read-char=? ,(char-downcase ch) ,(char-upcase ch))
                      `(read-char=? ,ch))
                 ,(generate-parser-rec #t t)
                 return))))
      (return V))))

(define (generate-parser)
  (let ((trie (make-trie)))
    (for-each (lambda (w)
                (trie-put! trie (string->list (car w)) (cdr w)))
              words)
    (generate-parser-rec #f trie)))


;;; parser macros

(load "lib.scm")
(load "churchnum.scm")

(defsyntax (defword num)
  (receive (m n)
           (quotient&remainder num 1000)
     `(cons (lambda (f0 f1 f2 f3)
              ,(string->symbol (string-append "f" (number->string m))))
            ,(string->symbol (string-append "c" (number->string n))))))

(defmacro (word? word) (word (lambda (_ _) I)))
(defmacro (motion? word) ((car word) I V V V))
(defmacro (noun? word) ((car word) V I V V))
(defmacro (verb? word) ((car word) V V I V))
(defmacro (special-word? word) ((car word) V V V I))
(defmacro (word-id-of word) (cdr word))

(add-unl-macro!
 'generated-parser '() (generate-parser))

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

(defmacro (word-data wordtbl word)
  (car ((word-id-of word) cdr (wordtbl (car word)))))

(defmacro def-motion-table V)
(defmacro def-noun-table V)
(defmacro def-verb-table V)
(defmacro def-special-word-table
  (list V  ; 0
        V  ; 1
        V  ; 2
        V  ; 3
        V  ; 4
        V  ; 5
        V  ; 6
        V  ; 7
        V  ; 8
        V  ; 9
        V  ; 10
        V  ; 11
        V  ; 12
        V  ; 13
        V  ; 14
        V  ; 15
        V  ; 16
        V  ; 17
        V  ; 18
        V  ; 19
        V  ; 20
        V  ; 21
        V  ; 22
        V  ; 23
        V  ; 24
        V  ; 25
        V  ; 26
        V  ; 27
        V  ; 28
        V  ; 29
        V  ; 30
        V  ; 31
        V  ; 32
        V  ; 33
        V  ; 34
        V  ; 35
        V  ; 36
        V  ; 37
        V  ; 38
        V  ; 39
        V  ; 40
        V  ; 41
        V  ; 42
        V  ; 43
        V  ; 44
        V  ; 45
        V  ; 46
        V  ; 47
        V  ; 48
        V  ; 49
        (string "GOOD TRY, BUT THAT IS AN OLD WORN-OUT MAGIC WORD.\n")  ; 50
        (string "help-string\n")  ; 51
        V  ; 52
        V  ; 53
        V  ; 54
        V  ; 55
        V  ; 56
        V  ; 57
        V  ; 58
        V  ; 59
        V  ; 60
        V  ; 61
        V  ; 62
        V  ; 63
        (string "tree-string\n")  ; 64
        V  ; 65
        (string "DDIGGING WITHOUT A SHOVEL IS QUITE IMPRACTICAL.  EVEN WITH A SHOVEL\nPROGRESS IS UNLIKELY.\n")  ; 66
        V  ; 67
        (string "I'M AS CONFUSED AS YOU ARE.\n")  ; 68
        (string "MIST IS A WHITE VAPOR, USUALLY WATER, SEEN FROM TIME TO TIME IN\nCAVERNS.  IT CAN BE FOUND ANYWHERE BUT IS FREQUENTLY A SIGN OF A DEEP\nPIT LEADING DOWN TO WATER.\n")  ; 69
        V  ; 70
        V  ; 71
        V  ; 72
        V  ; 73
        V  ; 74
        V  ; 75
        V  ; 76
        V  ; 77
        V  ; 78
        (string "WATCH IT!\n")  ; 79
        V  ; 80
        V  ; 81
        V  ; 82
        V  ; 83
        V  ; 84
        V  ; 85
        V  ; 86
        V  ; 87
        V  ; 88
        V  ; 89
        V  ; 90
        V  ; 91
        V  ; 92
        V  ; 93
        V  ; 94
        V  ; 95
        V  ; 96
        V  ; 97
        V  ; 98
        V  ; 99
        V  ; 100
        V  ; 101
        V  ; 102
        V  ; 103
        V  ; 104
        V  ; 105
        V  ; 106
        V  ; 107
        V  ; 108
        V  ; 109
        V  ; 110
        V  ; 111
        V  ; 112
        V  ; 113
        V  ; 114
        V  ; 115
        V  ; 116
        V  ; 117
        V  ; 118
        V  ; 119
        V  ; 120
        V  ; 121
        V  ; 122
        V  ; 123
        V  ; 124
        V  ; 125
        V  ; 126
        V  ; 127
        V  ; 128
        V  ; 129
        V  ; 130
        V  ; 131
        V  ; 132
        V  ; 133
        V  ; 134
        V  ; 135
        V  ; 136
        V  ; 137
        V  ; 138
        (string "I DON'T KNOW THE WORD \"STOP\".  USE \"QUIT\" IF YOU WANT TO GIVE UP.\n")  ; 139
        V  ; 140
        V  ; 141
        (string "info-string\n")  ; 142
        V  ; 143
        V  ; 144
        V  ; 145
        V  ; 146
        (string "I DON'T KNOW HOW.\n")  ; 147
        ))

(defmacro def-word-table
  (lambda (f)
    (f def-motion-table
       def-noun-table
       def-verb-table
       def-special-word-table)))

(defmacro getin
  ((lambda (parser)
     (call/cc
      (lambda (return)
        ((@ I)
         skip-spaces
         ((?newline I) return V)  ; empty input
         (let ((word1 (call/cc parser)))
           (if (word? word1)
               (skip-spaces
                ((?newline I) return (cons word1 V))
                (let ((word2 (call/cc parser)))
                  (if (word? word2)
                      (skip-spaces
                       ((?newline I) return (cons word1 word2))
                       (skip-until-newline)
                       (return (print$ "Please stick to 1- and 2-word commands.\n" V)))  ; more than 2 words
                    ((skip-until-newline) (return (cons V V))))))  ; unknown word
             ((skip-until-newline) (return (cons V V)))))))))  ; unknown word
   generated-parser))

(defmacro getin-loop
  (call/cc
   (lambda (return)
     ((lambda (x) (x x))
      (lambda (rec)
        ((#\> I)
         (let ((words getin))
           (((words (lambda (hd tl) I)) return words)
            (rec rec)))))))))

(defmacro main
  (call/cc
   (lambda (q)
     ((getin-loop
      (lambda (w1 w2)
        ((print-digit (word-id-of w1) I)
         (#\space I)
         (if (word? w2)
             (print-digit (word-id-of w2) I)
           ((string "none") I))
         (q I))))
      ((string "what?") I)))))

;main
