#!/usr/bin/env gosh
(require "enum.scm")

(define max-loc (lookup-enum 'didit))

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
                 ((= d 0) `(cons I ,e))
                 (else `(,(churchnum d) (cons V) (cons I ,e)))))))
   'V
   (cons -1 (sort (map lookup-enum words)))))

;; env -> env
(define (motion-code cond dest)
  (if (string? dest)
      `(lambda (world) ((string ,dest) world))
      `(lambda (world)
         (set-newloc world (lambda (_) ,dest)))))

(define (make-inst dest cond words)
  `(cons ,(motion-match words)
         ,(motion-code cond dest)))

(defmacro (inst-match inst) (car inst))
(defmacro (inst-code inst) (cdr inst))

(define (cond-not obj prop)
  (+ 300 (lookup-enum obj) (* 100 prop)))

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

(define-location 'slit
  "At your feet all the water of the stream splashes into a 2-inch slit\n\
in the rock.  Downstream the streambed is bare rock."
  "You're at slit in streambed."
  '(lighted liquid)
  (make-inst 'road 0 '(HOUSE))
  (make-inst 'valley 0 '(UPSTREAM N))
  (make-inst 'forest 0 '(WOODS E W))
  (make-inst 'outside 0 '(DOWNSTREAM ROCK BED S))
  (make-inst "You don't fit through a two-inch slit!" 0 '(SLIT STREAM D))
  )

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
  (make-inst "You can't go through a locked steel grate!" 0 '(ENTER))
  )


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
           (apply-inst tl world2))))))
