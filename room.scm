#!/usr/bin/env gosh
(require "enum.scm")

(define max-loc (lookup-enum 'didit))
(defmacro max-loc didit)

(define long-desc (make-vector (+ 1 max-loc)))
(define short-desc (make-vector (+ 1 max-loc)))
(define loc-flags (make-vector (+ 1 max-loc)))
(define travels (make-vector (+ 1 max-loc)))

(define (define-location name ldesc sdesc flags . insts)
  (let ((n (lookup-enum name)))
    (vector-set! long-desc n ldesc)
    (vector-set! short-desc n sdesc)
    (vector-set! loc-flags n flags)
    (vector-set! travels n insts)
    ))

(define (motion-match words)
  (pair-fold-right
   (lambda (lis e)
     (if (null? (cdr lis))
         'V
         (let ((d (- (cadr lis) (car lis) 1)))
           (cond ((< d 0) (error "duplicate motion" words))
                 ((= d 0) `(icons I ,e))
                 (else `(,(churchnum d) (icons V) (icons I ,e)))))))
   'V
   (cons -1 (sort (map lookup-enum words)))))

;; env -> env
(define (motion-code condition dest)
  (cond ((zero? condition)
         `(lambda (world)
            ,(if (string? dest)
                 `((string ,(string-append dest "\n")) world)
                 `(set-newloc world (lambda (_) ,dest)))))
        ((= 50 condition)
         `(lambda (world)
            (let ((world2 (set-rand world (lambda (r) (cdr r)))))
              (set-newloc world2 (lambda (_) ((car (rand world)) V ,dest))))))
        (else
         `(lambda (world)
            (set-newloc world (lambda (_) V))))))

(define (make-inst dest cond words)
  `(icons ,(motion-match words)
          ,(motion-code cond dest)))

(defmacro (inst-match inst) (car inst))
(defmacro (inst-code inst) (cdr inst))

(define (cond-holds obj)
  (+ 100 (lookup-enum obj)))
(define (cond-sees obj)
  (+ 200 (lookup-enum obj)))
(define (cond-not obj prop)
  (+ 300 (lookup-enum obj) (* 100 prop)))

(define all-alike "You are in a maze of twisty little passages, all alike.")
(define dead-end "Dead end.")

(define-location 'road
  "You are standing at the end of a road before a small brick building.\n\
Around you is a forest.  A small stream flows out of the building and\n\
down a gully."
  "You're at end of road again."
  '(lighted liquid)
  (make-inst 'hill 0 '(W U ROAD))
  (make-inst 'house 0 '(E IN HOUSE ENTER))
  (make-inst 'valley 0 '(S D GULLY STREAM DOWNSTREAM))
  (make-inst 'forest 0 '(N WOODS))
  (make-inst 'outside 0 '(DEPRESSION))
  )

(define-location 'hill
  "You have walked up a hill, still in the forest.  The road slopes back\n\
down the other side of the hill.  There is a building in the distance."
  "You're at hill in road."
  '(lighted)
  (make-inst 'road 0 '(ROAD HOUSE FORWARD E D))
  (make-inst 'forest 0 '(WOODS N S))
  )

(define-location 'house
  "You are inside a building, a well house for a large spring."
  "You're inside building."
  '(lighted liquid)
  (make-inst 'road 0 '(ENTER OUT OUTDOORS W))
  (make-inst 'debris 0 '(XYZZY))
  (make-inst 'y2 0 '(PLUGH))
  (make-inst 'sewer 0 '(DOWNSTREAM STREAM))
  )

(define-location 'valley
  "You are in a valley in the forest beside a stream tumbling along a\n\
rocky bed."
  "You're in valley."
  '(lighted liquid)
  (make-inst 'road 0 '(UPSTREAM HOUSE N))
  (make-inst 'forest 0 '(WOODS E W U))
  (make-inst 'slit 0 '(DOWNSTREAM S D))
  (make-inst 'outside 0 '(DEPRESSION))
  )

(define-location 'forest
  "You are in open forest, with a deep valley to one side."
  "You're in forest."
  '(lighted)
  (make-inst 'valley 0 '(VALLEY E D))
  (make-inst 'forest 50 '(WOODS FORWARD N))
  (make-inst 'woods 0 '(WOODS))
  (make-inst 'forest 0 '(W S))
  )

(define-location 'woods
  "You are in open forest near both a valley and a road."
  "You're in forest."
  '(lighted)
  (make-inst 'road 0 '(ROAD N))
  (make-inst 'valley 0 '(VALLEY E W D))
  (make-inst 'forest 0 '(WOODS S))
  )

(define slit-rmk "You don't fit through a two-inch slit!")
(define-location 'slit
  "At your feet all the water of the stream splashes into a 2-inch slit\n\
in the rock.  Downstream the streambed is bare rock."
  "You're at slit in streambed."
  '(lighted liquid)
  (make-inst 'road 0 '(HOUSE))
  (make-inst 'valley 0 '(UPSTREAM N))
  (make-inst 'forest 0 '(WOODS E W))
  (make-inst 'outside 0 '(DOWNSTREAM ROCK BED S))
  (make-inst slit-rmk 0 '(SLIT STREAM D))
  )

(define grate-rmk "You can't go through a locked steel grate!")
(define-location 'outside
  "You are in a 20-foot depression floored with bare dirt.  Set into the\n\
dirt is a strong steel grate mounted in concrete.  A dry streambed\n\
leads into the depression."
  "You're outside grate."
  '(lighted cave_hint)
  (make-inst 'forest 0 '(WOODS E W S))
  (make-inst 'road 0 '(HOUSE))
  (make-inst 'slit 0 '(UPSTREAM GULLY N))
  (make-inst 'inside (cond-not 'GRATE 0) '(ENTER IN D))
  (make-inst grate-rmk 0 '(ENTER))
  )

(define-location 'inside
  "You are in a small chamber beneath a 3x3 steel grate to the surface.\n\
A low crawl over cobbles leads inwards to the west."
  "You're below the grate."
  '(lighted)
  (make-inst 'outside (cond-not 'GRATE 0) '(OUT U))
  (make-inst grate-rmk 0 '(OUT))
  (make-inst 'cobbles 0 '(CRAWL COBBLES IN W))
  (make-inst 'spit 0 '(PIT))
  (make-inst 'debris 0 '(DEBRIS))
  )

(define-location 'cobbles
  "You are crawling over cobbles in a low passage.  There is a dim light\n\
at the east end of the passage."
  "You're in cobble crawl."
  '(lighted)
  (make-inst 'inside 0 '(OUT SURFACE NOWHERE E))
  (make-inst 'debris 0 '(IN DARK W DEBRIS))
  (make-inst 'spit 0 '(PIT))
  )

(define-location 'debris
  "You are in a debris room filled with stuff washed in from the surface.\n\
A low wide passage with cobbles becomes plugged with mud and debris\n\
here, but an awkward canyon leads upward and west.  A note on the wall\n\
says \"MAGIC WORD XYZZY\"."
  "You're in debris room."
  '()
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'cobbles 0 '(CRAWL COBBLES PASSAGE LOW E))
  (make-inst 'awk 0 '(CANYON IN U W))
  (make-inst 'house 0 '(XYZZY))
  (make-inst 'spit 0 '(PIT))
  )

(define-location 'awk
  "You are in an awkward sloping east/west canyon."
  "You are in an awkward sloping east/west canyon."
  '()
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'debris 0 '(D E DEBRIS))
  (make-inst 'bird 0 '(IN U W))
  (make-inst 'spit 0 '(PIT))
  )

(define-location 'bird
  "You are in a splendid chamber thirty feet high.  The walls are frozen\n\
rivers of orange stone.  An awkward canyon and a good passage exit\n\
from east and west sides of the chamber."
  "You're in bird chamber."
  '(bird_hint)
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'debris 0 '(DEBRIS))
  (make-inst 'awk 0 '(CANYON E))
  (make-inst 'spit 0 '(PASSAGE PIT W))
  )

(define-location 'spit
  "At your feet is a small pit breathing traces of white mist.  An east\n\
passage ends here except for a small crack leading on."
  "You're at top of small pit."
  '()
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'debris 0 '(DEBRIS))
  (make-inst 'bird 0 '(PASSAGE E))
  (make-inst 'neck (cond-holds 'GOLD) '(D PIT STEPS))
  (make-inst 'emist 0 '(D))
  (make-inst 'crack 0 '(CRACK W))
  )

(define-location 'emist
  "You are at one end of a vast hall stretching forward out of sight to\n\
the west.  There are openings to either side.  Nearby, a wide stone\n\
staircase leads downward.  The hall is filled with wisps of white mist\n\
swaying to and fro almost as if alive.  A cold wind blows up the\n\
staircase.  There is a passage at the top of a dome behind you."
  "You're in Hall of Mists."
  '()
  (make-inst 'nugget 0 '(L S))
  (make-inst 'efiss 0 '(FORWARD HALL W))
  (make-inst 'hmk 0 '(STAIRS D N))
  (make-inst 'cant (cond-holds 'GOLD) '(U PIT STEPS DOME PASSAGE E))
  (make-inst 'spit 0 '(U))
  (make-inst 'jumble 0 '(Y2))
  )

(define-location 'nugget
  "This is a low room with a crude note on the wall.  The note says,\n\
\"You won't get it up the steps\"."
  "You're in nugget of gold room."
  '()
  (make-inst 'emist 0 '(HALL OUT N))
  )

(define bridge-rmk
  "I respectfully suggest you go across the bridge instead of jumping.")
(define-location 'efiss
  "You are on the east bank of a fissure slicing clear across the hall.\n\
The mist is quite thick here, and the fissure is too wide to jump."
  "You're on east bank of fissure."
  '()
  (make-inst 'emist 0 '(HALL E))
  (make-inst bridge-rmk (cond-not 'CRYSTAL 0) '(JUMP))
  (make-inst 'lose (cond-not 'CRYSTAL 1) '(FORWARD))
  (make-inst "There is no way across the fissure." (cond-not 'CRYSTAL 1) '(OVER ACROSS W CROSS))
  (make-inst 'wfiss 0 '(OVER))
  )

(define-location 'wfiss
  "You are on the west side of the fissure in the Hall of Mists."
  "You are on the west side of the fissure in the Hall of Mists."
  '()
  (make-inst bridge-rmk (cond-not 'CRYSTAL 0) '(JUMP))
  (make-inst 'lose (cond-not 'CRYSTAL 1) '(FORWARD))
  (make-inst "There is no way across the fissure." (cond-not 'CRYSTAL 1) '(OVER ACROSS E CROSS))
  (make-inst 'efiss 0 '(OVER))
  (make-inst 'thru 0 '(N))
  (make-inst 'wmist 0 '(W))
  )

(define-location 'wmist
  "You are at the west end of the Hall of Mists.  A low wide crawl\n\
continues west and another goes north.  To the south is a little\n\
passage 6 feet off the floor."
  "You're at west end of Hall of Mists."
  '()
  (make-inst 'like1 0 '(S U PASSAGE CLIMB))
  (make-inst 'wfiss 0 '(E))
  (make-inst 'duck 0 '(N))
  (make-inst 'elong 0 '(W CRAWL))
  )

(define-location 'like1 all-alike all-alike '(twist_hint)
  (make-inst 'wmist 0 '(U))
  (make-inst 'like1 0 '(N))
  (make-inst 'like2 0 '(E))
  (make-inst 'like4 0 '(S))
  (make-inst 'like11 0 '(W))
  )

(define-location 'like2 all-alike all-alike '(twist_hint)
  (make-inst 'like1 0 '(W))
  (make-inst 'like3 0 '(S))
  (make-inst 'like4 0 '(E))
  )

(define-location 'like3 all-alike all-alike '(twist_hint)
  (make-inst 'like2 0 '(E))
  (make-inst 'dead5 0 '(D))
  (make-inst 'like6 0 '(S))
  (make-inst 'dead9 0 '(N))
  )

(define-location 'like4 all-alike all-alike '(twist_hint)
  (make-inst 'like1 0 '(W))
  (make-inst 'like2 0 '(N))
  (make-inst 'dead3 0 '(E))
  (make-inst 'dead4 0 '(S))
  (make-inst 'like14 0 '(U D))
  )

(define-location 'like5 all-alike all-alike '(twist_hint)
  (make-inst 'like6 0 '(E))
  (make-inst 'like7 0 '(W))
  )

(define-location 'like6 all-alike all-alike '(twist_hint)
  (make-inst 'like3 0 '(E))
  (make-inst 'like5 0 '(W))
  (make-inst 'like7 0 '(D))
  (make-inst 'like8 0 '(S))
  )

(define-location 'like7 all-alike all-alike '(twist_hint)
  (make-inst 'like5 0 '(W))
  (make-inst 'like6 0 '(U))
  (make-inst 'like8 0 '(E))
  (make-inst 'like9 0 '(S))
  )

(define-location 'like8 all-alike all-alike '(twist_hint)
  (make-inst 'like6 0 '(W))
  (make-inst 'like7 0 '(E))
  (make-inst 'like8 0 '(S))
  (make-inst 'like9 0 '(U))
  (make-inst 'like10 0 '(N))
  (make-inst 'dead11 0 '(D))
  )

(define-location 'like9 all-alike all-alike '(twist_hint)
  (make-inst 'like7 0 '(W))
  (make-inst 'like8 0 '(N))
  (make-inst 'dead6 0 '(S))
  )

(define-location 'like10 all-alike all-alike '(twist_hint)
  (make-inst 'like8 0 '(W))
  (make-inst 'like10 0 '(N))
  (make-inst 'dead7 0 '(D))
  (make-inst 'brink 0 '(E))
  )

(define-location 'like11 all-alike all-alike '(twist_hint)
  (make-inst 'like1 0 '(N))
  (make-inst 'like11 0 '(W S))
  (make-inst 'dead1 0 '(E))
  )

(define-location 'like12 all-alike all-alike '(twist_hint)
  (make-inst 'brink 0 '(S))
  (make-inst 'like13 0 '(E))
  (make-inst 'dead10 0 '(W))
  )

(define-location 'like13 all-alike all-alike '(twist_hint)
  (make-inst 'brink 0 '(N))
  (make-inst 'like12 0 '(W))
  (make-inst 'dead2 0 '(NW))
  )

(define-location 'like14 all-alike all-alike '(twist_hint)
  (make-inst 'like4 0 '(U D))
  )

(define-location 'brink
  "You are on the brink of a thirty-foot pit with a massive orange column\n\
down one wall.  You could climb down here but you could not get back\n\
up.  The maze continues at this level."
  "You're at brink of pit."
  '()
  (make-inst 'bird 0 '(D CLIMB))
  (make-inst 'like10 0 '(W))
  (make-inst 'dead8 0 '(S))
  (make-inst 'like12 0 '(N))
  (make-inst 'like13 0 '(E))
  )

(define-location 'elong
  "You are at the east end of a very long hall apparently without side\n\
chambers.  To the east a low wide crawl slants up.  To the north a\n\
round two-foot hole slants down."
  "You're at east end of long hall."
  '()
  (make-inst 'wmist 0 '(E U CRAWL))
  (make-inst 'wlong 0 '(W))
  (make-inst 'cross 0 '(N D HOLE))
  )

(define-location 'wlong
  "You are at the west end of a very long featureless hall.  The hall\n\
joins up with a narrow north/south passage."
  "You're at west end of long hall."
  '()
  (make-inst 'elong 0 '(E))
  (make-inst 'cross 0 '(N))
  (make-inst 'diff0 100 '(S))
  )

(define (define-twist l n s e w ne se nw sw u d m)
  (define-location l m m '()
    (make-inst n 0 '(N))
    (make-inst s 0 '(S))
    (make-inst e 0 '(E))
    (make-inst w 0 '(W))
    (make-inst ne 0 '(NE))
    (make-inst se 0 '(SE))
    (make-inst nw 0 '(NW))
    (make-inst sw 0 '(SW))
    (make-inst u 0 '(U))
    (make-inst d 0 '(D))
    ))

(define-twist 'diff0 'diff9 'diff1 'diff7 'diff8 'diff3 'diff4 'diff6 'diff2 'diff5 'wlong
  "You are in a maze of twisty little passages, all different.")

(define-twist 'diff1 'diff8 'diff9 'diff10 'diff0 'diff5 'diff2 'diff3 'diff4 'diff6 'diff7
  "You are in a maze of twisting little passages, all different.")

(define-twist 'diff2 'diff3 'diff4 'diff8 'diff5 'diff7 'diff10 'diff0 'diff6 'diff1 'diff9
  "You are in a little maze of twisty passages, all different.")

(define-twist 'diff3 'diff7 'diff10 'diff6 'diff2 'diff4 'diff9 'diff8 'diff5 'diff0 'diff1
  "You are in a twisting maze of little passages, all different.")

(define-twist 'diff4 'diff1 'diff7 'diff5 'diff9 'diff0 'diff3 'diff2 'diff10 'diff8 'diff6
  "You are in a twisting little maze of passages, all different.")

(define-twist 'diff5 'diff0 'diff3 'diff4 'diff6 'diff8 'diff1 'diff9 'diff7 'diff10 'diff2
  "You are in a twisty little maze of passages, all different.")

(define-twist 'diff6 'diff10 'diff5 'diff0 'diff1 'diff9 'diff8 'diff7 'diff3 'diff2 'diff4
  "You are in a twisty maze of little passages, all different.")

(define-twist 'diff7 'diff6 'diff2 'diff9 'diff10 'diff1 'diff0 'diff5 'diff8 'diff4 'diff3
  "You are in a little twisty maze of passages, all different.")

(define-twist 'diff8 'diff5 'diff6 'diff1 'diff4 'diff2 'diff7 'diff10 'diff9 'diff3 'diff0
  "You are in a maze of little twisting passages, all different.")

(define-twist 'diff9 'diff4 'diff8 'diff2 'diff3 'diff10 'diff6 'diff1 'diff0 'diff7 'diff5
  "You are in a maze of little twisty passages, all different.")

(define-twist 'diff10 'diff2 'pony 'diff3 'diff7 'diff6 'diff5 'diff4 'diff1 'diff9 'diff8
  "You are in a little maze of twisting passages, all different.")

(add-unl-macro!
 'initial-long-desc '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list long-desc))))

(add-unl-macro!
 'initial-short-desc '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list short-desc))))

(add-unl-macro!
 'travels '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V `(list ,@x)))
               (vector->list travels))))

(defrecmacro (find-inst motion table)
  (table
   (lambda (hd tl)
     (if (nth motion (inst-match hd))
         table
         (find-inst motion tl)))))

(defrecmacro (apply-inst table world)
  (table
   (lambda (hd tl)
     (let ((world2 ((inst-code hd) world)))
       (if ((newloc world2) I I)
           world2
           (apply-inst tl (set-newloc world2 (lambda (_) (newloc world)))))))))

(defmacro initial-visits
  (max-loc (cons V) (cons V V)))
