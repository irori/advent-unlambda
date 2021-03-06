(define-module room
  (use gauche.sequence)
  (use gauche.record)
  (use unlc)
  (use lib)
  (use enum)
  (use rand)
  (use object)
  )
(select-module room)

(defmacro max-loc didit)
(defmacro min-in-cave inside)
(defmacro min-lower-loc emist)
(defmacro min-forced-loc crack)
(defmacro max-pirate-loc dead2)
(defmacro chest-loc dead2)
(defmacro message-loc pony)
(define max-loc (lookup-enum 'didit))
(define min-lower-loc (lookup-enum 'emist))
(define min-forced-loc (lookup-enum 'crack))

(define room-desc (make-vector (+ 1 max-loc) #f))
(define loc-flags (make-vector (+ 1 max-loc)))
(define travels (make-vector (+ 1 max-loc) #f))

(define-record-type inst #t #t
  dest cond words)

(define (define-location name ldesc sdesc flags . insts)
  (let ((n (lookup-enum name)))
    (vector-set! room-desc n (cons ldesc sdesc))
    (vector-set! loc-flags n flags)
    (vector-set! travels n insts)
    ))

(define (cond-holds obj)
  (+ 100 (lookup-enum obj)))
(define (cond-sees obj)
  (+ 200 (lookup-enum obj)))
(define (cond-not obj prop)
  (+ 300 (lookup-enum obj) (* 100 prop)))

(define all-alike "You are in a maze of twisty little passages, all alike.")
(define dead-end "Dead end.")

(define-location 'road
  "You are standing at the end of a road before a small brick building.
Around you is a forest.  A small stream flows out of the building and
down a gully."
  "You're at end of road again."
  '(lighted liquid)
  (make-inst 'hill 0 '(W U ROAD))
  (make-inst 'house 0 '(E IN HOUSE ENTER))
  (make-inst 'valley 0 '(SOUTH D GULLY STREAM DOWNSTREAM))
  (make-inst 'forest 0 '(N WOODS))
  (make-inst 'outside 0 '(DEPRESSION))
  )

(define-location 'hill
  "You have walked up a hill, still in the forest.  The road slopes back
down the other side of the hill.  There is a building in the distance."
  "You're at hill in road."
  '(lighted)
  (make-inst 'road 0 '(ROAD HOUSE FORWARD E D))
  (make-inst 'forest 0 '(WOODS N SOUTH))
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
  "You are in a valley in the forest beside a stream tumbling along a
rocky bed."
  "You're in valley."
  '(lighted liquid)
  (make-inst 'road 0 '(UPSTREAM HOUSE N))
  (make-inst 'forest 0 '(WOODS E W U))
  (make-inst 'slit 0 '(DOWNSTREAM SOUTH D))
  (make-inst 'outside 0 '(DEPRESSION))
  )

(define-location 'forest
  "You are in open forest, with a deep valley to one side."
  "You're in forest."
  '(lighted)
  (make-inst 'valley 0 '(VALLEY E D))
  (make-inst 'forest 50 '(WOODS FORWARD N))
  (make-inst 'woods 0 '(WOODS))
  (make-inst 'forest 0 '(W SOUTH))
  )

(define-location 'woods
  "You are in open forest near both a valley and a road."
  "You're in forest."
  '(lighted)
  (make-inst 'road 0 '(ROAD N))
  (make-inst 'valley 0 '(VALLEY E W D))
  (make-inst 'forest 0 '(WOODS SOUTH))
  )

(define slit-rmk "You don't fit through a two-inch slit!")
(define-location 'slit
  "At your feet all the water of the stream splashes into a 2-inch slit
in the rock.  Downstream the streambed is bare rock."
  "You're at slit in streambed."
  '(lighted liquid)
  (make-inst 'road 0 '(HOUSE))
  (make-inst 'valley 0 '(UPSTREAM N))
  (make-inst 'forest 0 '(WOODS E W))
  (make-inst 'outside 0 '(DOWNSTREAM ROCK BED SOUTH))
  (make-inst slit-rmk 0 '(SLIT STREAM D))
  )

(define grate-rmk "You can't go through a locked steel grate!")
(define-location 'outside
  "You are in a 20-foot depression floored with bare dirt.  Set into the
dirt is a strong steel grate mounted in concrete.  A dry streambed
leads into the depression."
  "You're outside grate."
  '(lighted cave-hint)
  (make-inst 'forest 0 '(WOODS E W SOUTH))
  (make-inst 'road 0 '(HOUSE))
  (make-inst 'slit 0 '(UPSTREAM GULLY N))
  (make-inst 'inside (cond-not 'GRATE 0) '(ENTER IN D))
  (make-inst grate-rmk 0 '(ENTER))
  )

(define-location 'inside
  "You are in a small chamber beneath a 3x3 steel grate to the surface.
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
  "You are crawling over cobbles in a low passage.  There is a dim light
at the east end of the passage."
  "You're in cobble crawl."
  '(lighted)
  (make-inst 'inside 0 '(OUT SURFACE NOWHERE E))
  (make-inst 'debris 0 '(IN DARK W DEBRIS))
  (make-inst 'spit 0 '(PIT))
  )

(define-location 'debris
  "You are in a debris room filled with stuff washed in from the surface.
A low wide passage with cobbles becomes plugged with mud and debris
here, but an awkward canyon leads upward and west.  A note on the wall
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
  #f
  '()
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'debris 0 '(D E DEBRIS))
  (make-inst 'bird 0 '(IN U W))
  (make-inst 'spit 0 '(PIT))
  )

(define-location 'bird
  "You are in a splendid chamber thirty feet high.  The walls are frozen
rivers of orange stone.  An awkward canyon and a good passage exit
from east and west sides of the chamber."
  "You're in bird chamber."
  '(bird-hint)
  (make-inst 'outside (cond-not 'GRATE 0) '(DEPRESSION))
  (make-inst 'inside 0 '(ENTRANCE))
  (make-inst 'debris 0 '(DEBRIS))
  (make-inst 'awk 0 '(CANYON E))
  (make-inst 'spit 0 '(PASSAGE PIT W))
  )

(define-location 'spit
  "At your feet is a small pit breathing traces of white mist.  An east
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
  "You are at one end of a vast hall stretching forward out of sight to
the west.  There are openings to either side.  Nearby, a wide stone
staircase leads downward.  The hall is filled with wisps of white mist
swaying to and fro almost as if alive.  A cold wind blows up the
staircase.  There is a passage at the top of a dome behind you."
  "You're in Hall of Mists."
  '()
  (make-inst 'nugget 0 '(L SOUTH))
  (make-inst 'efiss 0 '(FORWARD HALL W))
  (make-inst 'hmk 0 '(STAIRS D N))
  (make-inst 'cant (cond-holds 'GOLD) '(U PIT STEPS DOME PASSAGE E))
  (make-inst 'spit 0 '(U))
  (make-inst 'jumble 0 '(Y2))
  )

(define-location 'nugget
  "This is a low room with a crude note on the wall.  The note says,
\"You won't get it up the steps\"."
  "You're in nugget of gold room."
  '()
  (make-inst 'emist 0 '(HALL OUT N))
  )

(define bridge-rmk
  "I respectfully suggest you go across the bridge instead of jumping.")
(define-location 'efiss
  "You are on the east bank of a fissure slicing clear across the hall.
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
  #f
  '()
  (make-inst bridge-rmk (cond-not 'CRYSTAL 0) '(JUMP))
  (make-inst 'lose (cond-not 'CRYSTAL 1) '(FORWARD))
  (make-inst "There is no way across the fissure." (cond-not 'CRYSTAL 1) '(OVER ACROSS E CROSS))
  (make-inst 'efiss 0 '(OVER))
  (make-inst 'thru 0 '(N))
  (make-inst 'wmist 0 '(W))
  )

(define-location 'wmist
  "You are at the west end of the Hall of Mists.  A low wide crawl
continues west and another goes north.  To the south is a little
passage 6 feet off the floor."
  "You're at west end of Hall of Mists."
  '()
  (make-inst 'like1 0 '(SOUTH U PASSAGE CLIMB))
  (make-inst 'wfiss 0 '(E))
  (make-inst 'duck 0 '(N))
  (make-inst 'elong 0 '(W CRAWL))
  )

(define-location 'like1 all-alike #f '(twist-hint)
  (make-inst 'wmist 0 '(U))
  (make-inst 'like1 0 '(N))
  (make-inst 'like2 0 '(E))
  (make-inst 'like4 0 '(SOUTH))
  (make-inst 'like11 0 '(W))
  )

(define-location 'like2 all-alike #f '(twist-hint)
  (make-inst 'like1 0 '(W))
  (make-inst 'like3 0 '(SOUTH))
  (make-inst 'like4 0 '(E))
  )

(define-location 'like3 all-alike #f '(twist-hint)
  (make-inst 'like2 0 '(E))
  (make-inst 'dead5 0 '(D))
  (make-inst 'like6 0 '(SOUTH))
  (make-inst 'dead9 0 '(N))
  )

(define-location 'like4 all-alike #f '(twist-hint)
  (make-inst 'like1 0 '(W))
  (make-inst 'like2 0 '(N))
  (make-inst 'dead3 0 '(E))
  (make-inst 'dead4 0 '(SOUTH))
  (make-inst 'like14 0 '(U D))
  )

(define-location 'like5 all-alike #f '(twist-hint)
  (make-inst 'like6 0 '(E))
  (make-inst 'like7 0 '(W))
  )

(define-location 'like6 all-alike #f '(twist-hint)
  (make-inst 'like3 0 '(E))
  (make-inst 'like5 0 '(W))
  (make-inst 'like7 0 '(D))
  (make-inst 'like8 0 '(SOUTH))
  )

(define-location 'like7 all-alike #f '(twist-hint)
  (make-inst 'like5 0 '(W))
  (make-inst 'like6 0 '(U))
  (make-inst 'like8 0 '(E))
  (make-inst 'like9 0 '(SOUTH))
  )

(define-location 'like8 all-alike #f '(twist-hint)
  (make-inst 'like6 0 '(W))
  (make-inst 'like7 0 '(E))
  (make-inst 'like8 0 '(SOUTH))
  (make-inst 'like9 0 '(U))
  (make-inst 'like10 0 '(N))
  (make-inst 'dead11 0 '(D))
  )

(define-location 'like9 all-alike #f '(twist-hint)
  (make-inst 'like7 0 '(W))
  (make-inst 'like8 0 '(N))
  (make-inst 'dead6 0 '(SOUTH))
  )

(define-location 'like10 all-alike #f '(twist-hint)
  (make-inst 'like8 0 '(W))
  (make-inst 'like10 0 '(N))
  (make-inst 'dead7 0 '(D))
  (make-inst 'brink 0 '(E))
  )

(define-location 'like11 all-alike #f '(twist-hint)
  (make-inst 'like1 0 '(N))
  (make-inst 'like11 0 '(W SOUTH))
  (make-inst 'dead1 0 '(E))
  )

(define-location 'like12 all-alike #f '(twist-hint)
  (make-inst 'brink 0 '(SOUTH))
  (make-inst 'like13 0 '(E))
  (make-inst 'dead10 0 '(W))
  )

(define-location 'like13 all-alike #f '(twist-hint)
  (make-inst 'brink 0 '(N))
  (make-inst 'like12 0 '(W))
  (make-inst 'dead2 0 '(NW))
  )

(define-location 'like14 all-alike #f '(twist-hint)
  (make-inst 'like4 0 '(U D))
  )

(define-location 'brink
  "You are on the brink of a thirty-foot pit with a massive orange column
down one wall.  You could climb down here but you could not get back
up.  The maze continues at this level."
  "You're at brink of pit."
  '()
  (make-inst 'bird 0 '(D CLIMB))
  (make-inst 'like10 0 '(W))
  (make-inst 'dead8 0 '(SOUTH))
  (make-inst 'like12 0 '(N))
  (make-inst 'like13 0 '(E))
  )

(define-location 'elong
  "You are at the east end of a very long hall apparently without side
chambers.  To the east a low wide crawl slants up.  To the north a
round two-foot hole slants down."
  "You're at east end of long hall."
  '()
  (make-inst 'wmist 0 '(E U CRAWL))
  (make-inst 'wlong 0 '(W))
  (make-inst 'cross 0 '(N D HOLE))
  )

(define-location 'wlong
  "You are at the west end of a very long featureless hall.  The hall
joins up with a narrow north/south passage."
  "You're at west end of long hall."
  '()
  (make-inst 'elong 0 '(E))
  (make-inst 'cross 0 '(N))
  (make-inst 'diff0 100 '(SOUTH))
  )

(define (define-twist l n s e w ne se nw sw u d m)
  (define-location l m #f '()
    (make-inst n 0 '(N))
    (make-inst s 0 '(SOUTH))
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

(define-location 'pony dead-end #f '()
  (make-inst 'diff10 0 '(N OUT)))

(define-location 'cross
  "You are at a crossover of a high N/S passage and a low E/W one."
  #f
  '()
  (make-inst 'elong 0 '(W))
  (make-inst 'dead0 0 '(N))
  (make-inst 'west 0 '(E))
  (make-inst 'wlong 0 '(SOUTH))
  )

(define-location 'hmk
  "You are in the Hall of the Mountain King, with passages off in all
directions."
  "You're in Hall of Mt King."
  '(snake-hint)
  (make-inst 'emist 0 '(STAIRS U E))
  (make-inst 'ns (cond-not 'SNAKE 0) '(N L))
  (make-inst 'south (cond-not 'SNAKE 0) '(SOUTH R))
  (make-inst 'west (cond-not 'SNAKE 0) '(W FORWARD))
  (make-inst 'snaked 0 '(N))
  (make-inst 'secret 35 '(SW))
  (make-inst 'snaked (cond-sees 'SNAKE) '(SW))
  (make-inst 'secret 0 '(SECRET))
  )

(define-location 'west
  "You are in the west side chamber of the Hall of the Mountain King.
A passage continues west and up here."
  "You're in west side chamber."
  '()
  (make-inst 'hmk 0 '(HALL OUT E))
  (make-inst 'cross 0 '(W U))
  )

(define-location 'south
  "You are in the south side chamber."
  #f
  '()
  (make-inst 'hmk 0 '(HALL OUT N))
  )

(define-location 'ns
  "You are in a low N/S passage at a hole in the floor.  The hole goes
down to an E/W passage."
  "You're in N/S passage."
  '()
  (make-inst 'hmk 0 '(HALL OUT SOUTH))
  (make-inst 'y2 0 '(N Y2))
  (make-inst 'dirty 0 '(D HOLE))
  )

(define-location 'y2
  "You are in a large room, with a passage to the south, a passage to the
west, and a wall of broken rock to the east.  There is a large \"Y2\" on
a rock in the room's center."
  "You're at \"Y2\"."
  '()
  (make-inst 'house 0 '(PLUGH))
  (make-inst 'ns 0 '(SOUTH))
  (make-inst 'jumble 0 '(E WALL BROKEN))
  (make-inst 'windoe 0 '(W))
  (make-inst 'pdrop (cond-holds 'EMERALD) '(PLOVER))
  (make-inst 'proom 0 '(PLOVER))
  )

(define-location 'jumble
  "You are in a jumble of rock, with cracks everywhere."
  #f
  '()
  (make-inst 'y2 0 '(D Y2))
  (make-inst 'emist 0 '(U))
  )

(define-location 'windoe
  "You're at a low window overlooking a huge pit, which extends up out of
sight.  A floor is indistinctly visible over 50 feet below.  Traces of
white mist cover the floor of the pit, becoming thicker to the right.
Marks in the dust around the window would seem to indicate that
someone has been here recently.  Directly across the pit from you and
25 feet away there is a similar window looking into a lighted room.
A shadowy figure can be seen there peering back at you."
  "You're at window on pit."
  '()
  (make-inst 'y2 0 '(E Y2))
  (make-inst 'neck 0 '(JUMP))
  )

(define-location 'dirty
  "You are in a dirty broken passage.  To the east is a crawl.  To the
west is a large passage.  Above you is a hole to another passage."
  "You're in dirty passage."
  '()
  (make-inst 'clean 0 '(E CRAWL))
  (make-inst 'ns 0 '(U HOLE))
  (make-inst 'dusty 0 '(W))
  (make-inst 'bedquilt 0 '(BEDQUILT))
  )

(define-location 'clean
  "You are on the brink of a small clean climbable pit.  A crawl leads
west."
  "You're by a clean pit."
  '()
  (make-inst 'dirty 0 '(W CRAWL))
  (make-inst 'wet 0 '(D PIT CLIMB))
  )

(define-location 'wet
  "You are in the bottom of a small pit with a little stream, which
enters and exits through tiny slits."
  "You're in pit by stream."
  '(liquid)
  (make-inst 'clean 0 '(CLIMB U OUT))
  (make-inst slit-rmk 0 '(SLIT STREAM D UPSTREAM DOWNSTREAM))
  )

(define-location 'dusty
  "You are in a large room full of dusty rocks.  There is a big hole in
the floor.  There are cracks everywhere, and a passage leading east."
  "You're in dusty rock room."
  '()
  (make-inst 'dirty 0 '(E PASSAGE))
  (make-inst 'complex 0 '(D HOLE FLOOR))
  (make-inst 'bedquilt 0 '(BEDQUILT))
  )

(define-location 'complex
  "You are at a complex junction.  A low hands-and-knees passage from the
north joins a higher crawl from the east to make a walking passage
going west.  There is also a large room above.  The air is damp here."
  "You're at complex junction."
  '()
  (make-inst 'dusty 0 '(U CLIMB ROOM))
  (make-inst 'bedquilt 0 '(W BEDQUILT))
  (make-inst 'shell 0 '(N SHELL))
  (make-inst 'ante 0 '(E))
  )

(define-location 'shell
  "You're in a large room carved out of sedimentary rock.  The floor
and walls are littered with bits of shells embedded in the stone.
A shallow passage proceeds downward, and a somewhat steeper one
leads up.  A low hands-and-knees passage enters from the south."
  "You're in Shell Room."
  '()
  (make-inst 'arch 0 '(U HALL))
  (make-inst 'ragged 0 '(D))
  (make-inst "You can't fit this five-foot clam through that little passage!" (cond-holds 'CLAM) '(SOUTH))
  (make-inst "You can't fit this five-foot oyster through that little passage!" (cond-holds 'OYSTER) '(SOUTH))
  (make-inst 'complex 0 '(SOUTH))
  )

(define-location 'arch
  "You are in an arched hall.  A coral passage once continued up and east
from here, but is now blocked by debris.  The air smells of sea water."
  "You're in arched hall."
  '()
  (make-inst 'shell 0 '(D SHELL OUT))
  )

(define-location 'ragged
  "You are in a long sloping corridor with ragged sharp walls."
  #f
  '()
  (make-inst 'shell 0 '(U SHELL))
  (make-inst 'sac 0 '(D))
  )

(define-location 'sac
  "You are in a cul-de-sac about eight feet across."
  #f
  '()
  (make-inst 'ragged 0 '(U OUT))
  (make-inst 'shell 0 '(SHELL))
  )

(define-location 'ante
  "You are in an anteroom leading to a large passage to the east.  Small
passages go west and up.  The remnants of recent digging are evident.
A sign in midair here says \"CAVE UNDER CONSTRUCTION BEYOND THIS POINT.
PROCEED AT OWN RISK.  [WITT CONSTRUCTION COMPANY]\""
  "You're in anteroom."
  '()
  (make-inst 'complex 0 '(U))
  (make-inst 'bedquilt 0 '(W))
  (make-inst 'witt 0 '(E))
  )

(define loop-rmk
  "You have crawled around in some little holes and wound up back in the
main passage.")

(define-location 'witt
  "You are at Witt's End.  Passages lead off in \"all\" directions."
  "You're at Witt's End."
  '(witt-hint)
  (make-inst loop-rmk 95 '(E N SOUTH NE SE SW NW U D))
  (make-inst 'ante 0 '(E))
  (make-inst "You have crawled around in some little holes and found your way
blocked by a recent cave-in.  You are now back in the main passage." 0 '(W))
  )

(define-location 'bedquilt
  "You are in Bedquilt, a long east/west passage with holes everywhere.
To explore at random select north, south, up, or down."
  "You're in Bedquilt."
  '()
  (make-inst 'complex 0 '(E))
  (make-inst 'cheese 0 '(W))
  (make-inst loop-rmk 80 '(SOUTH))
  (make-inst 'slab 0 '(SLAB))
  (make-inst loop-rmk 80 '(U))
  (make-inst 'abovep 50 '(U))
  (make-inst 'dusty 0 '(U))
  (make-inst loop-rmk 60 '(N))
  (make-inst 'low 75 '(N))
  (make-inst 'sjunc 0 '(N))
  (make-inst loop-rmk 80 '(D))
  (make-inst 'ante 0 '(D))
  )

(define-location 'cheese
  "You are in a room whose walls resemble Swiss cheese.  Obvious passages
go west, east, NE, and NW.  Part of the room is occupied by a large
bedrock block."
  "You're in Swiss cheese room."
  '()
  (make-inst 'bedquilt 0 '(NE))
  (make-inst 'e2pit 0 '(W))
  (make-inst loop-rmk 80 '(SOUTH))
  (make-inst 'tall 0 '(CANYON))
  (make-inst 'soft 0 '(E))
  (make-inst loop-rmk 50 '(NW))
  (make-inst 'oriental 0 '(ORIENTAL))
  )

(define-location 'soft
  "You are in the Soft Room.  The walls are covered with heavy curtains,
the floor with a thick pile carpet.  Moss covers the ceiling."
  "You're in Soft Room."
  '()
  (make-inst 'cheese 0 '(W OUT))
  )

(define-location 'e2pit
  "You are at the east end of the Twopit Room.  The floor here is
littered with thin rock slabs, which make it easy to descend the pits.
There is a path here bypassing the pits to connect passages from east
and west.  There are holes all over, but the only big one is on the
wall directly over the west pit where you can't get to it."
  "You're at east end of Twopit Room."
  '()
  (make-inst 'cheese 0 '(E))
  (make-inst 'w2pit 0 '(W ACROSS))
  (make-inst 'epit 0 '(D PIT))
  )

(define-location 'w2pit
  "You are at the west end of the Twopit Room.  There is a large hole in
the wall above the pit at this end of the room."
  "You're at west end of Twopit Room."
  '()
  (make-inst 'e2pit 0 '(E ACROSS))
  (make-inst 'slab 0 '(W SLAB))
  (make-inst 'wpit 0 '(D PIT))
  (make-inst "It is too far up for you to reach." 0 '(HOLE))
  )

(define-location 'epit
  "You are at the bottom of the eastern pit in the Twopit Room.  There is
a small pool of oil in one corner of the pit."
  "You're in east pit."
  '(liquid oil)
  (make-inst 'e2pit 0 '(U OUT))
  )

(define-location 'wpit
  "You are at the bottom of the western pit in the Twopit Room.  There is
a large hole in the wall about 25 feet above you."
  "You're in west pit."
  '()
  (make-inst 'w2pit 0 '(U OUT))
  (make-inst 'check (cond-not 'PLANT 4) '(CLIMB))
  (make-inst 'climb 0 '(CLIMB))
  )

(define-location 'narrow
  "You are in a long, narrow corridor stretching out of sight to the
west.  At the eastern end is a hole through which you can see a
profusion of leaves."
  "You're in narrow corridor."
  '()
  (make-inst 'wpit 0 '(D CLIMB E))
  (make-inst 'neck 0 '(JUMP))
  (make-inst 'giant 0 '(W GIANT))
  )

(define-location 'giant
  "You are in the Giant Room.  The ceiling here is too high up for your
lamp to show it.  Cavernous passages lead east, north, and south.  On
the west wall is scrawled the inscription, \"FEE FIE FOE FOO\" [sic]."
  "You're in Giant Room."
  '()
  (make-inst 'narrow 0 '(SOUTH))
  (make-inst 'block 0 '(E))
  (make-inst 'immense 0 '(N))
  )

(define-location 'block
  "The passage here is blocked by a recent cave-in."
  #f
  '()
  (make-inst 'giant 0 '(SOUTH GIANT OUT))
  )

(define-location 'immense
  "You are at one end of an immense north/south passage."
  #f
  '()
  (make-inst 'giant 0 '(SOUTH GIANT PASSAGE))
  (make-inst 'falls (cond-not 'DOOR 0) '(N ENTER CAVERN))
  (make-inst "The door is extremely rusty and refuses to open." 0 '(N))
  )

(define-location 'falls
  "You are in a magnificent cavern with a rushing stream, which cascades
over a sparkling waterfall into a roaring whirlpool that disappears
through a hole in the floor.  Passages exit to the south and west."
  "You're in cavern with waterfall."
  '(liquid)
  (make-inst 'immense 0 '(SOUTH OUT))
  (make-inst 'giant 0 '(GIANT))
  (make-inst 'steep 0 '(W))
  )

(define-location 'steep
  "You are at the top of a steep incline above a large room.  You could
climb down here, but you would not be able to climb up.  There is a
passage leading back to the north."
  "You're at steep incline above large room."
  '()
  (make-inst 'falls 0 '(N CAVERN PASSAGE))
  (make-inst 'low 0 '(D CLIMB))
  )

(define-location 'abovep
  "You are in a secret N/S canyon above a sizable passage."
  #f
  '()
  (make-inst 'sjunc 0 '(N))
  (make-inst 'bedquilt 0 '(D PASSAGE))
  (make-inst 'tite 0 '(SOUTH))
  )

(define-location 'sjunc
  "You are in a secret canyon at a junction of three canyons, bearing
north, south, and SE.  The north one is as tall as the other two
combined."
  "You're at junction of three secret canyons."
  '()
  (make-inst 'bedquilt 0 '(SE))
  (make-inst 'abovep 0 '(SOUTH))
  (make-inst 'window 0 '(N))
  )

(define-location 'tite
  "A large stalactite extends from the roof and almost reaches the floor
below.  You could climb down it, and jump from it to the floor, but
having done so you would be unable to reach it to climb back up."
  "You're on top of stalactite."
  '()
  (make-inst 'abovep 0 '(N))
  (make-inst 'like6 40 '(D JUMP CLIMB))
  (make-inst 'like9 50 '(D))
  (make-inst 'like4 0 '(D))
  )

(define-location 'low
  "You are in a large low room.  Crawls lead north, SE, and SW."
  #f
  '()
  (make-inst 'bedquilt 0 '(BEDQUILT))
  (make-inst 'scorr 0 '(SW))
  (make-inst 'crawl 0 '(N))
  (make-inst 'oriental 0 '(SE ORIENTAL))
  )

(define-location 'crawl
  "Dead end crawl."
  #f
  '()
  (make-inst 'low 0 '(SOUTH CRAWL OUT))
  )

(define-location 'window
  "You're at a low window overlooking a huge pit, which extends up out of
sight.  A floor is indistinctly visible over 50 feet below.  Traces of
white mist cover the floor of the pit, becoming thicker to the left.
Marks in the dust around the window would seem to indicate that
someone has been here recently.  Directly across the pit from you and
25 feet away there is a similar window looking into a lighted room.
A shadowy figure can be seen there peering back at you."
  "You're at window on pit."
  '()
  (make-inst 'sjunc 0 '(W))
  (make-inst 'neck 0 '(JUMP))
  )

(define-location 'oriental
  "This is the Oriental Room.  Ancient oriental cave drawings cover the
walls.  A gently sloping passage leads upward to the north, another
passage leads SE, and a hands-and-knees crawl leads west."
  "You're in Oriental Room."
  '()
  (make-inst 'cheese 0 '(SE))
  (make-inst 'low 0 '(W CRAWL))
  (make-inst 'misty 0 '(U N CAVERN))
  )

(define-location 'misty
  "You are following a wide path around the outer edge of a large cavern.
Far below, through a heavy white mist, strange splashing noises can be
heard.  The mist rises up through a fissure in the ceiling.  The path
exits to the south and west."
  "You're in misty cavern."
  '()
  (make-inst 'oriental 0 '(SOUTH ORIENTAL))
  (make-inst 'alcove 0 '(W))
  )

(define-location 'alcove
  "You are in an alcove.  A small NW path seems to widen after a short
distance.  An extremely tight tunnel leads east.  It looks like a very
tight squeeze.  An eerie light can be seen at the other end."
  "You're in alcove."
  '(dark-hint)
  (make-inst 'misty 0 '(NW CAVERN))
  (make-inst 'ppass 0 '(E PASSAGE))
  (make-inst 'proom 0 '(E))
  )

(define-location 'proom
  "You're in a small chamber lit by an eerie green light.  An extremely
narrow tunnel exits to the west.  A dark corridor leads NE."
  "You're in Plover Room."
  '(lighted dark-hint)
  (make-inst 'ppass 0 '(W PASSAGE OUT))
  (make-inst 'alcove 0 '(W))
  (make-inst 'pdrop (cond-holds 'EMERALD) '(PLOVER))
  (make-inst 'y2 0 '(PLOVER))
  (make-inst 'droom 0 '(NE DARK))
  )

(define-location 'droom
  "You're in the Dark-Room.  A corridor leading south is the only exit."
  "You're in Dark-Room."
  '(dark-hint)
  (make-inst 'proom 0 '(SOUTH PLOVER OUT))
  )

(define-location 'slab
  "You are in a large low circular chamber whose floor is an immense slab
fallen from the ceiling (Slab Room).  There once were large passages
to the east and west, but they are now filled with boulders.  Low
small passages go north and south, and the south one quickly bends
east around the boulders."
  "You're in Slab Room."
  '()
  (make-inst 'w2pit 0 '(SOUTH))
  (make-inst 'abover 0 '(U CLIMB))
  (make-inst 'bedquilt 0 '(N))
  )

(define-location 'abover
  "You are in a secret N/S canyon above a large room."
  #f
  '()
  (make-inst 'slab 0 '(D SLAB))
  (make-inst 'scan2 (cond-not 'DRAGON 0) '(SOUTH))
  (make-inst 'scan1 0 '(SOUTH))
  (make-inst 'mirror 0 '(N))
  (make-inst 'res 0 '(RESERVOIR))
  )

(define-location 'mirror
  "You are in a north/south canyon about 25 feet across.  The floor is
covered by white mist seeping in from the north.  The walls extend
upward for well over 100 feet.  Suspended from some unseen point far
above you, an enormous two-sided mirror is hanging parallel to and
midway between the canyon walls.  (The mirror is obviously provided
for the use of the dwarves, who as you know are extremely vain.)
A small window can be seen in either wall, some fifty feet up."
  "You're in mirror canyon."
  '()
  (make-inst 'abover 0 '(SOUTH))
  (make-inst 'res 0 '(N RESERVOIR))
  )

(define-location 'res
  "You are at the edge of a large underground reservoir.  An opaque cloud
of white mist fills the room and rises rapidly upward.  The lake is
fed by a stream, which tumbles out of a hole in the wall about 10 feet
overhead and splashes noisily into the water somewhere within the
mist.  The only passage goes back toward the south."
  "You're at reservoir."
  '(liquid)
  (make-inst 'mirror 0 '(SOUTH OUT))
  )

(define-location 'scan1
  "You are in a secret canyon that exits to the north and east."
  #f
  '()
  (make-inst 'abover 0 '(N OUT))
  (make-inst "The dragon looks rather nasty.  You'd best not try to get by." 0 '(E FORWARD))
  )

(define-location 'scan2
  "You are in a secret canyon that exits to the north and east."
  #f
  '()
  (make-inst 'abover 0 '(N))
  (make-inst 'secret 0 '(E))
  )

(define-location 'scan3
  "You are in a secret canyon that exits to the north and east."
  #f
  '()
  (make-inst 'secret 0 '(E OUT))
  (make-inst "The dragon looks rather nasty.  You'd best not try to get by." 0 '(N FORWARD))
  )

(define-location 'secret
  "You are in a secret canyon, which here runs E/W.  It crosses over a
very tight canyon 15 feet below.  If you go down you may not be able
to get back up."
  "You're in secret E/W canyon above tight canyon."
  '()
  (make-inst 'hmk 0 '(E))
  (make-inst 'scan2 (cond-not 'DRAGON 0) '(W))
  (make-inst 'scan3 0 '(W))
  (make-inst 'wide 0 '(D))
  )

(define-location 'wide
  "You are at a wide place in a very tight N/S canyon."
  #f
  '()
  (make-inst 'tight 0 '(SOUTH))
  (make-inst 'tall 0 '(N))
  )

(define-location 'tight
  "The canyon here becomes too tight to go further south."
  #f
  '()
  (make-inst 'wide 0 '(N))
  )

(define-location 'tall
  "You are in a tall E/W canyon.  A low tight crawl goes 3 feet north and
seems to open up."
  "You're in tall E/W canyon."
  '()
  (make-inst 'wide 0 '(E))
  (make-inst 'boulders 0 '(W))
  (make-inst 'cheese 0 '(N CRAWL))
  )

(define-location 'boulders
  "The canyon runs into a mass of boulders --- dead end."
  #f
  '()
  (make-inst 'tall 0 '(SOUTH))
  )

(define-location 'scorr
  "You are in a long winding corridor sloping out of sight in both
directions."
  "You're in sloping corridor."
  '()
  (make-inst 'low 0 '(D))
  (make-inst 'swside 0 '(U))
  )

(define-location 'swside
  "You are on one side of a large, deep chasm.  A heavy white mist rising
up from below obscures all view of the far side.  A SW path leads away
from the chasm into a winding corridor."
  "You're on SW side of chasm."
  '()
  (make-inst 'scorr 0 '(SW))
  (make-inst "The troll refuses to let you cross." (cond-sees 'TROLL) '(OVER ACROSS CROSS NE))
  (make-inst "There is no longer any way across the chasm." (cond-not 'BRIDGE 0) '(OVER))
  (make-inst 'troll 0 '(OVER))
  (make-inst 'lose (cond-not 'BRIDGE 0) '(JUMP))
  (make-inst bridge-rmk 0 '(JUMP))
  )

(define-location 'dead0 dead-end #f '()
  (make-inst 'cross 0 '(SOUTH OUT)))

(define-location 'dead1 dead-end #f '(twist-hint)
  (make-inst 'like11 0 '(W OUT)))

(define-location 'dead2 dead-end #f '()
  (make-inst 'like13 0 '(SE)))

(define-location 'dead3 dead-end #f '(twist-hint)
  (make-inst 'like4 0 '(W OUT)))

(define-location 'dead4 dead-end #f '(twist-hint)
  (make-inst 'like4 0 '(E OUT)))

(define-location 'dead5 dead-end #f '(twist-hint)
  (make-inst 'like3 0 '(U OUT)))

(define-location 'dead6 dead-end #f '(twist-hint)
  (make-inst 'like9 0 '(W OUT)))

(define-location 'dead7 dead-end #f '(twist-hint)
  (make-inst 'like10 0 '(U OUT)))

(define-location 'dead8 dead-end #f '()
  (make-inst 'brink 0 '(E OUT)))

(define-location 'dead9 dead-end #f '(twist-hint)
  (make-inst 'like3 0 '(SOUTH OUT)))

(define-location 'dead10 dead-end #f '(twist-hint)
  (make-inst 'like12 0 '(E OUT)))

(define-location 'dead11 dead-end #f '(twist-hint)
  (make-inst 'like8 0 '(U OUT)))


(define-location 'neside
  "You are on the far side of the chasm.  A NE path leads away from the
chasm on this side."
  "You're on NE side of chasm."
  '()
  (make-inst 'corr 0 '(NE))
  (make-inst "The troll refuses to let you cross." (cond-sees 'TROLL) '(OVER ACROSS CROSS SW))
  (make-inst 'troll 0 '(OVER))
  (make-inst bridge-rmk 0 '(JUMP))
  (make-inst 'fork 0 '(FORK))
  (make-inst 'view 0 '(VIEW))
  (make-inst 'fbarr 0 '(BARREN))
  )

(define-location 'corr
  "You're in a long east/west corridor.  A faint rumbling noise can be
heard in the distance."
  "You're in corridor."
  '()
  (make-inst 'neside 0 '(W))
  (make-inst 'fork 0 '(E FORK))
  (make-inst 'view 0 '(VIEW))
  (make-inst 'fbarr 0 '(BARREN))
  )

(define-location 'fork
  "The path forks here.  The left fork leads northeast.  A dull rumbling
seems to get louder in that direction.  The right fork leads southeast
down a gentle slope.  The main corridor enters from the west."
  "You're at fork in path."
  '()
  (make-inst 'corr 0 '(W))
  (make-inst 'warm 0 '(NE L))
  (make-inst 'lime 0 '(SE R D))
  (make-inst 'view 0 '(VIEW))
  (make-inst 'fbarr 0 '(BARREN))
  )

(define-location 'warm
  "The walls are quite warm here.  From the north can be heard a steady
roar, so loud that the entire cave seems to be trembling.  Another
passage leads south, and a low crawl goes east."
  "You're at junction with warm walls."
  '()
  (make-inst 'fork 0 '(SOUTH FORK))
  (make-inst 'view 0 '(N VIEW))
  (make-inst 'chamber 0 '(E CRAWL))
  )

(define-location 'view
  "You are on the edge of a breath-taking view.  Far below you is an
active volcano, from which great gouts of molten lava come surging
out, cascading back down into the depths.  The glowing rock fills the
farthest reaches of the cavern with a blood-red glare, giving every-
thing an eerie, macabre appearance.  The air is filled with flickering
sparks of ash and a heavy smell of brimstone.  The walls are hot to
the touch, and the thundering of the volcano drowns out all other
sounds.  Embedded in the jagged roof far overhead are myriad twisted
formations, composed of pure white alabaster, which scatter the murky
light into sinister apparitions upon the walls.  To one side is a deep
gorge, filled with a bizarre chaos of tortured rock that seems to have
been crafted by the Devil himself.  An immense river of fire crashes
out from the depths of the volcano, burns its way through the gorge,
and plummets into a bottomless pit far off to your left.  To the
right, an immense geyser of blistering steam erupts continuously
from a barren island in the center of a sulfurous lake, which bubbles
ominously.  The far right wall is aflame with an incandescence of its
own, which lends an additional infernal splendor to the already
hellish scene.  A dark, foreboding passage exits to the south."
  "You're at breath-taking view."
  '(lighted)
  (make-inst 'warm 0 '(SOUTH PASSAGE OUT))
  (make-inst 'fork 0 '(FORK))
  (make-inst "Don't be ridiculous!" 0 '(D JUMP))
  )

(define-location 'chamber
  "You are in a small chamber filled with large boulders.  The walls are
very warm, causing the air in the room to be almost stifling from the
heat.  The only exit is a crawl heading west, through which a low
rumbling noise is coming."
  "You're in chamber of boulders."
  '()
  (make-inst 'warm 0 '(W OUT CRAWL))
  (make-inst 'fork 0 '(FORK))
  (make-inst 'view 0 '(VIEW))
  )

(define-location 'lime
  "You are walking along a gently sloping north/south passage lined with
oddly shaped limestone formations."
  "You're in limestone passage."
  '()
  (make-inst 'fork 0 '(N U FORK))
  (make-inst 'fbarr 0 '(SOUTH D BARREN))
  (make-inst 'view 0 '(VIEW))
  )

(define-location 'fbarr
  "You are standing at the entrance to a large, barren room.  A sign
posted above the entrance reads:  \"CAUTION!  BEAR IN ROOM!\""
  "You're in front of barren room."
  '()
  (make-inst 'lime 0 '(W U))
  (make-inst 'fork 0 '(FORK))
  (make-inst 'barr 0 '(E IN BARREN ENTER))
  (make-inst 'view 0 '(VIEW))
  )

(define-location 'barr
  "You are inside a barren room.  The center of the room is completely
empty except for some dust.  Marks in the dust lead away toward the
far end of the room.  The only exit is the way you came in."
  "You're in barren room."
  '()
  (make-inst 'fbarr 0 '(W OUT))
  (make-inst 'fork 0 '(FORK))
  (make-inst 'view 0 '(VIEW))
  )

(define-location 'neend
  "You are at the northeast end of an immense room, even larger than the
Giant Room.  It appears to be a repository for the \"Adventure\"
program.  Massive torches far overhead bathe the room with smoky
yellow light.  Scattered about you can be seen a pile of bottles (all
of them empty), a nursery of young beanstalks murmuring quietly, a bed
of oysters, a bundle of black rods with rusty stars on their ends, and
a collection of brass lanterns.  Off to one side a great many dwarves
are sleeping on the floor, snoring loudly.  A sign nearby reads:  \"DO
NOT DISTURB THE DWARVES!\"  An immense mirror is hanging against one
wall, and stretches to the other end of the room, where various other
sundry objects can be glimpsed dimly in the distance."
  "You're at NE end."
  '(lighted)
  (make-inst 'swend 0 '(SW))
  )

(define-location 'swend
  "You are at the southwest end of the repository.  To one side is a pit
full of fierce green snakes.  On the other side is a row of small
wicker cages, each of which contains a little sulking bird.  In one
corner is a bundle of black rods with rusty marks on their ends.
A large number of velvet pillows are scattered about on the floor.
A vast mirror stretches off to the northeast.  At your feet is a
large steel grate, next to which is a sign that reads, \"TREASURE
VAULT.  KEYS IN MAIN OFFICE.\""
  "You're at SW end."
  '(lighted)
  (make-inst 'neend 0 '(NE))
  (make-inst grate-rmk 0 '(D))
  )

(define-location 'crack
  "The crack is far too small for you to follow."
  #f
  '()
  (make-inst 'spit 0 'force)
  )

(define-location 'neck
  "You are at the bottom of the pit with a broken neck."
  #f
  '()
  (make-inst 'limbo 0 'force)
  )

(define-location 'lose "You didn't make it." #f '()
  (make-inst 'limbo 0 'force))

(define-location 'cant
  "The dome is unclimbable."
  #f
  '()
  (make-inst 'emist 0 'force)
  )

(define-location 'climb
  "You clamber up the plant and scurry through the hole at the top."
  #f
  '()
  (make-inst 'narrow 0 'force)
  )

(define-location 'check #f #f '()
  (make-inst 'upnout (cond-not 'PLANT 2) 'force)
  (make-inst 'didit 0 'force)
  )

(define-location 'snaked
  "You can't get by the snake."
  #f
  '()
  (make-inst 'hmk 0 'force)
  )

(define-location 'thru
  "You have crawled through a very low wide passage parallel to and north
of the Hall of Mists."
  #f
  '()
  (make-inst 'wmist 0 'force)
  )

(define-location 'duck
  "You have crawled through a very low wide passage parallel to and north
of the Hall of Mists."
  #f
  '()
  (make-inst 'wfiss 0 'force)
  )

(define-location 'sewer
  "The stream flows out through a pair of 1-foot-diameter sewer pipes.
It would be advisable to use the exit."
  #f
  '()
  (make-inst 'house 0 'force)
  )

(define-location 'upnout
  "There is nothing here to climb.  Use \"up\" or \"out\" to leave the pit."
  #f
  '()
  (make-inst 'wpit 0 'force)
  )

(define-location 'didit
  "You have climbed up the plant and out of the pit."
  #f
  '()
  (make-inst 'w2pit 0 'force)
  )

(add-unl-macro!
 'room-desc '()
 (compile-to-file
  "roomdesc.unlo"
  (compress-list (map (lambda (desc)
			(cond ((not desc) 'V)
			      ((cdr desc) `(icons (string ,(car desc))
						  (string ,(cdr desc))))
			      ((car desc) `(lambda (x)
					     (string ,(car desc))))
			      (else 'V)))
		      room-desc))))

(define (compile-insts insts)
  (if insts
      (list 'icons
            (compile-travel-matcher insts)
            (compress-list (map motion-code insts)))
      'V))

(define (compile-travel-matcher insts)
  (if (eq? 'force (inst-words (car insts)))
      '(repeat c0)
      (let ((v (make-vector (+ 1 (lookup-enum 'NOWHERE)) 'V)))
        (for-each-with-index
         (lambda (i inst)
           (for-each
            (lambda (w)
              (if (eq? (vector-ref v w) 'V)
                  (vector-set! v w (churchnum i))))
            (map lookup-enum (inst-words inst))))
         insts)
        (compress-list (vector->list v)))))

;; env -> env
(define (motion-code inst)
  (let* ((condition (inst-cond inst))
         (dest (inst-dest inst))
         (result (if (string? dest)
                     `((string ,(string-append dest "\n")) (location world))
                     dest)))
    (cond ((zero? condition)
           `(lambda (world) ,result))
          ((and (< 0 condition) (< condition 100))
           `(lambda (world)
              (if (pct ,condition world) ,result V)))
          ((> condition 300)
           (let* ((obj (remainder condition 100))
                  (val (- (quotient condition 100) 3))
                  (prop-expr `(nth ,(churchnum obj) (prop world))))
             `(lambda (world)
                (if ,(cond ((zero? val)
                            `(zero? ,prop-expr))
                           ((= val 1)
                            `(nth ,prop-expr (list V I)))
                           (else
                            `(= ,(churchnum val) ,prop-expr)))
                    V ,result))))
          (else
           (let* ((obj (remainder condition 100))
		  (condition (if (>= condition 200)
				 `(or (toting? ,(churchnum obj) world)
				      (at-loc? ,(churchnum obj) world))
				 `(toting? ,(churchnum obj) world))))
             `(lambda (world)
                (if ,condition ,result V)))))))

(add-unl-macro!
 'travels '()
 (compile-to-file
  "travels.unlo"
  (compress-list (map compile-insts travels))))

(define (make-back-table here insts)
  (let ((lst '())
        (flst '()))
    (for-each
     (lambda (inst)
       (let ((dest (inst-dest inst))
             (word (car (inst-words inst))))
         (cond ((string? dest) #f)
               ((< (lookup-enum dest) min-forced-loc)
                (push! lst (cons dest word)))
               ((<= (lookup-enum dest) max-loc)
                (let ((dest2 (first-dest-of dest)))
                  (if (and (not (eq? dest2 'limbo))
                           (< (lookup-enum dest2) min-forced-loc))
                      (push! flst (cons (first-dest-of dest) word))
                      #f))))))
     insts)
    (let* ((alist (append (reverse lst) flst))
           (keys (filter! (lambda (x) (not (= here (lookup-enum x))))
                          (delete-duplicates! (map car alist)))))
      (map (lambda (key) (assq key alist))
           keys))))

(define (first-dest-of loc)
  (let ((insts (vector-ref travels (lookup-enum loc))))
    (inst-dest (car insts))))

(define (compile-back-table alist)
  `(lambda (l)
     (nth l ,(make-lookup-table alist))))

(define (print-back-table)
  (for-each-with-index
   (lambda (index insts)
     (if (or (not insts) (>= index min-forced-loc))
         #f
         (print (cons index (compile-back-table (make-back-table index insts))))))
   travels))

(add-unl-macro!
 'back-table '()
 (compile-to-file
  "backtbl.unlo"
  (compress-list (map-with-index
		  (lambda (i insts)
		    (if (or (not insts) (>= i min-forced-loc))
			'V
			(compile-back-table (make-back-table i insts))))
		  travels))))

(define (make-dwarf-moves here insts)
  (if (or (not insts) (< here min-lower-loc) (>= here min-forced-loc))
      '()
      (fold
       (lambda (inst a)
         (let* ((dest (inst-dest inst))
                (d (if (string? dest) 0 (lookup-enum dest))))
           (if (and (>= d min-lower-loc)
                    (not (= d here))
                    (or (null? a) (not (eq? dest (car a))))
                    (not (= (inst-cond inst) 100))
                    (< d min-forced-loc))
               (cons dest a)
               a)))
       '()
       insts)))

(define (print-dwarf-moves)
  (for-each-with-index
   (lambda (index insts)
     (if (or (not insts) (>= index min-forced-loc))
         #f
         (print (cons index (make-dwarf-moves index insts)))))
   travels))

(add-unl-macro!
 'dwarf-moves '()
 (compile-to-file
  "dmoves.unlo"
  (compress-list (map-with-index
                  (lambda (i insts)
                    (cons 'list (make-dwarf-moves i insts)))
                  travels))))

(add-unl-macro!
 'lighted-rooms '()
 (compress-list (map (lambda (flags) (if (memq 'lighted flags) 'I 'V))
		     loc-flags)))

(defmacro (lighted? loc)
  (nth loc lighted-rooms))

(add-unl-macro!
 'room-hint '()
 (compress-list (map
		 (lambda (flags)
		   (cond ((memq 'cave-hint flags) 'c0)
			 ((memq 'bird-hint flags) 'c1)
			 ((memq 'snake-hint flags) 'c2)
			 ((memq 'twist-hint flags) 'c3)
			 ((memq 'dark-hint flags) 'c4)
			 ((memq 'witt-hint flags) 'c5)
			 (else 'V)))
		 loc-flags)))

(add-unl-macro!
 'initial-liquid '()
 (compress-list (map (lambda (flags)
		       (cond ((memq 'oil flags) 'c1)
			     ((memq 'liquid flags) 'c0)
			     (else 'V)))
		     loc-flags)))

(defmacro (water-here world)
  (zero? (nth (location world) (liquid world))))

(defmacro (oil-here world)
  (nonzero? (nth (location world) (liquid world))))

(defmacro (no-liquid-here world)
  (not (churchnum? (nth (location world) (liquid world)))))

(defmacro (forced-move? loc)
  (>= loc min-forced-loc))

(defmacro (find-inst motion travel)
  (travel
   (lambda (mot-lookup lst)
     ((nth motion mot-lookup) cdr lst))))

(defrecmacro (apply-inst lst world)
  (lst
   (lambda (hd tl)
     (let ((nl (hd world)))
       (let-world (($set-rand cdr))
         (cond ((= pdrop nl)
                (let-world (($drop EMERALD $location))
                  (apply-inst tl world)))
               ((churchnum? nl)
                ($set-newloc (K nl)))
               (else
                (apply-inst tl world))))))))

(defmacro initial-visits
  (max-loc (cons V) (cons V V)))

; for debug
(add-unl-macro!
 'location-names ()
 (cons 'list
       (map (lambda (x) `(string ,(symbol->string x)))
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
              ppass pdrop troll))))
