#!/usr/bin/env gosh
(require "enum.scm")

(define max-loc (lookup-enum 'didit))

(define long-desc (make-vector (+ 1 max-loc)))
(define short-desc (make-vector (+ 1 max-loc)))
(define loc-flags (make-vector (+ 1 max-loc)))

(define (define-location name ldesc sdesc flags)
  (let ((n (lookup-enum name)))
    (vector-set! long-desc n ldesc)
    (vector-set! short-desc n sdesc)
    (vector-set! loc-flags n flags)))

(define-location 'road
  "You are standing at the end of a road before a small brick building.\n\
Around you is a forest.  A small stream flows out of the building and\n\
down a gully."
  "You're at end of road again."
  '(lighted liquid))
;make_inst(W,0,hill);ditto(U);ditto(ROAD);
;make_inst(E,0,house);ditto(IN);ditto(HOUSE);ditto(ENTER);
;make_inst(S,0,valley);ditto(D);ditto(GULLY);ditto(STREAM);
;ditto(DOWNSTREAM);
;make_inst(N,0,forest);ditto(WOODS);
;make_inst(DEPRESSION,0,outside);

(define-location 'hill
  "You have walked up a hill, still in the forest.  The road slopes back\n\
down the other side of the hill.  There is a building in the distance."
  "You're at hill in road."
  '(lighted))
;make_inst(ROAD,0,road);ditto(HOUSE);ditto(FORWARD);ditto(E);ditto(D);
;make_inst(WOODS,0,forest);ditto(N);ditto(S);

(define-location 'house
  "You are inside a building, a well house for a large spring."
  "You're inside building."
  '(lighted liquid))
;make_inst(ENTER,0,road);ditto(OUT);ditto(OUTDOORS);ditto(W);
;make_inst(XYZZY,0,debris);
;make_inst(PLUGH,0,y2);
;make_inst(DOWNSTREAM,0,sewer);ditto(STREAM);

(define-location 'valley
  "You are in a valley in the forest beside a stream tumbling along a\n\
rocky bed."
  "You're in valley."
  '(lighted liquid))
;make_inst(UPSTREAM,0,road);ditto(HOUSE);ditto(N);
;make_inst(WOODS,0,forest);ditto(E);ditto(W);ditto(U);
;make_inst(DOWNSTREAM,0,slit);ditto(S);ditto(D);
;make_inst(DEPRESSION,0,outside);

(define-location 'forest
  "You are in open forest, with a deep valley to one side."
  "You're in forest."
  '(lighted))
;make_inst(VALLEY,0,valley);ditto(E);ditto(D);
;make_inst(WOODS,50,forest);ditto(FORWARD);ditto(N);
;make_inst(WOODS,0,woods);
;make_inst(W,0,forest);ditto(S);

(define-location 'woods
  "You are in open forest near both a valley and a road."
  "You're in forest."
  '(lighted))
;make_inst(ROAD,0,road);ditto(N);
;make_inst(VALLEY,0,valley);ditto(E);ditto(W);ditto(D);
;make_inst(WOODS,0,forest);ditto(S);

(define-location 'slit
  "At your feet all the water of the stream splashes into a 2-inch slit\n\
in the rock.  Downstream the streambed is bare rock."
  "You're at slit in streambed."
  '(lighted liquid))
;make_inst(HOUSE,0,road);
;make_inst(UPSTREAM,0,valley);ditto(N);
;make_inst(WOODS,0,forest);ditto(E);ditto(W);
;make_inst(DOWNSTREAM,0,outside);ditto(ROCK);ditto(BED);ditto(S);
;remark("You don't fit through a two-inch slit!");
;make_inst(SLIT,0,sayit);ditto(STREAM);ditto(D);
;slit_rmk= sayit;

(define-location 'outside
  "You are in a 20-foot depression floored with bare dirt.  Set into the\n\
dirt is a strong steel grate mounted in concrete.  A dry streambed\n\
leads into the depression."
  "You're outside grate."
  '(lighted cave_hint))
;make_inst(WOODS,0,forest);ditto(E);ditto(W);ditto(S);
;make_inst(HOUSE,0,road);
;make_inst(UPSTREAM,0,slit);ditto(GULLY);ditto(N);
;make_inst(ENTER,not(GRATE,0),inside);ditto(ENTER);ditto(IN);ditto(D);
;remark("You can't go through a locked steel grate!");
;grate_rmk= sayit;
;make_inst(ENTER,0,sayit);


(add-unl-macro!
 'initial-long-desc '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list long-desc))))

(add-unl-macro!
 'initial-short-desc '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V (list 'string x)))
               (vector->list short-desc))))
