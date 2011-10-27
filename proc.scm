#!/usr/bin/env gosh
(require "enum.scm")

(defmacro goto cons)

(define procedures '())

(define (define-proc name body)
  (define-enum (list name) (length procedures))
  (set! procedures (cons body procedures)))

; 75 Simulate an adventure, going to quit when finished
(define-proc 'mainloop
  '(lambda (world)
     (let-world ((set-location world (lambda (_) (newloc world))))
       (goto commence world))))

; 141 Report the long description and continue
(defmacro handle-look
  (lambda (world)
    (let-world ((set-was-dark world (K V))
		(set-nth world set-visits (location world) (K V)))
      ((if (cons1? (1-of-1 (verbose world)))
	   ((string "Sorry, but I am not allowed to give more detail.  I will repeat the\nlong description of your location.\n")
	    (set-verbose world 1-of-1))
	   world)))))

; 75 try-move:
(define-proc 'try-move
  '(lambda (world)
     ((cond ((= (mot world) NOWHERE)
             (lambda (world1) (goto mainloop world1)))
            ((= (mot world) LOOK)
             (lambda (world1) (goto mainloop (handle-look world1))))
            (else
             (lambda (world1)
               (goto go-for-it
                     (set-oldlocs world1 (lambda (ol-ool)
                                           (cons (location world1)
                                                 (car ol-ool))))))))
      (set-newloc world (K (location world))))))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     (let-world ((set-verb world (K ABSTAIN))
		 (set-obj world (K NOTHING)))
       (goto cycle world))))

; 76 cycle:
(define-proc 'cycle
  '(lambda (world)
     (let ((words listen))
       (let-world ((set-was-dark world (K (dark world)))
		   (set-word12 world (lambda (_) words)))
	 (goto pre-parse world)))))

; 76 pre_parse:
(define-proc 'pre-parse
  '(lambda (world)
     ; turns++
     (let ((old-limit (limit world)))
       (let-world ((set-limit world (if (= (nth LAMP (prop world)) c1) 1-of-1 I))
		   (if (and (cons1? old-limit) (not (cons1? (limit world))))
		       (begin
			 ((here? LAMP world)
			  (string "Your lamp has run out of power.\n") I)
			 (set-nth world set-prop LAMP (K c0)))
		       world))
	 (goto look-at-word1 world)))))

; 76 shift:
(define-proc 'shift
  '(lambda (world)
     (goto look-at-word1 (set-word12 world (lambda (old) (icons (cdr old) V))))))

; Gee, I don't understand
(defmacro (unknown-word world)
  (set-rand
   world
   (lambda (bits)
     (bits
      (lambda (b)
        (b (string "I don't know that word.\n")
           (lambda (bits2)
             (bits2
              (lambda (b2)
                (b2 (string "What?\n")
                    (string "I don't understand that!\n")))))))))))

; 78 Look at word1 and exit to the right place if it completes a command
(define-proc 'look-at-word1
  '(lambda (world)
     ((word12 world)
      (lambda (word1 word2)
        (if (word? word1)
            ((word-type word1)
             (lambda (_)  ; motion
               (goto try-move (set-mot world (lambda (_) (word-meaning word1)))))
             (lambda (_)  ; object
               (let-world ((set-obj world (K (word-meaning word1))))
                 (if (or (toting? (obj world) world)
                         (at-loc? (obj world) world))
                     (goto handle-object-word world)
                     (goto cant-see-it world))))
             (lambda (_)  ; verb
	       (let-world ((set-verb world (K (word-meaning word1))))
		 (if (word? word2)
		     (goto shift world)
		     (goto (if (obj world) transitive intransitive)
			   world))))
             (lambda (_)  ; message
               (begin ((nth (word-meaning word1) (message world)) #\newline I)
                      (goto get-user-input world)))
             I)
            (goto cycle (unknown-word world)))))))

; 78
(define-proc 'handle-object-word
  '(lambda (world)
     (if (word? (cdr (word12 world)))
         (goto shift world)
         (if (nonzero? (verb world))
             (goto transitive world)
             (begin
               ((string "What do you want to do with the ") I)
               ((word-letters (car (word12 world))) I)
               ((string "?\n") I)
               (goto cycle world))))))

(define-proc 'intransitive
  '(lambda (world)
     (goto (nth (verb world)
                (list V
                      intransitive-take  ;TAKE
                      get-object  ;DROP
                      intransitive-open  ;OPEN
                      intransitive-open  ;CLOSE
                      transitive  ;ON
                      transitive  ;OFF
                      quit  ;WAVE
                      quit  ;CALM
                      report-default  ;GO
                      report-default  ;RELAX
                      transitive  ;POUR
                      intransitive-eat  ;EAT
                      transitive  ;DRINK
                      quit  ;RUB
                      quit  ;TOSS
                      quit  ;WAKE
                      quit  ;FEED
                      transitive  ;FILL
                      quit  ;BREAK
                      transitive  ;BLAST
                      transitive  ;KILL
                      quit  ;SAY
                      quit  ;READ
                      quit  ;FEEFIE
                      intransitive-brief  ;BRIEF
                      quit  ;FIND
                      intransitive-inventory  ;INVENTORY
                      quit  ;SCORE
                      quit  ;QUIT
                 ))
           world)))

(define-proc 'transitive
  '(lambda (world)
     (goto (nth (verb world)
                (list V
                      transitive-take  ;TAKE
                      transitive-drop  ;DROP
                      transitive-open  ;OPEN
                      transitive-open  ;CLOSE
                      transitive-on  ;ON
                      transitive-off  ;OFF
                      quit  ;WAVE
                      quit  ;CALM
                      quit  ;GO
                      quit  ;RELAX
                      quit  ;POUR
                      transitive-eat  ;EAT
                      quit  ;DRINK
                      quit  ;RUB
                      quit  ;TOSS
                      quit  ;WAKE
                      quit  ;FEED
                      quit  ;FILL
                      quit  ;BREAK
                      quit  ;BLAST
                      quit  ;KILL
                      quit  ;SAY
                      quit  ;READ
                      quit  ;FEEFIE
                      report-default  ;BRIEF
                      quit  ;FIND
                      quit  ;INVENTORY
                      quit  ;SCORE
                      quit  ;QUIT
                 ))
           world)))

; 79 report_default:
(define-proc 'report-default
  '(lambda (world)
     (begin
       ((nth (verb world) (default-msg world)) #\newline I)
       (goto get-user-input world))))

; 79 get_object:
(define-proc 'get-object
  '(lambda (world)
     (begin
       ((word-letters (car (word12 world))) I)
       ((string " what?\n") I)
       (goto cycle world))))

; 79 cant_see_it:
(define-proc 'cant-see-it
  '(lambda (world)
     (if (and (or (= (verb world) FIND) (= (verb world) INVENTORY))
              (not (word? (cdr (word12 world)))))
         (goto transitive world)
         (begin
           ((string "I see no ") I)
           ((word-letters (car (word12 world))) I)
           ((string " here.\n") I)
           (goto get-user-input world)))))

(defmacro (dark world)
  (and (not (lighted? (location world)))
       (or (zero? (nth LAMP (prop world)))
           (not (here? LAMP world)))))

(defmacro pitch-dark-msg
  (string "It is now pitch dark.  If you proceed you will most likely fall into a pit."))

; 86 Report the current state
(define-proc 'commence
  `(lambda (world)
     (let ((next (commence-sub world)))
       (if (and (dark world) (not (forced-move? (location world))))
           (next pitch-dark-msg)
           (let ((selector (if (cons1? (nth (location world) (visits world)))
                               ->cdr ->car)))
             (next ((nth (location world) room-desc) selector)))))))
         

(defmacro (increment-visits world)
  (set-nth world set-visits (location world)
           (lambda (x)
             (if (cons1? x)
                 (if (cons1? (verbose world))
                     (1-of-1 x)
                     x)
                 (cons1 (cons1 (cons1 (cons1 V))))))))

; 88 Describe the objects at this location
(define-proc 'describe-objects
  '(lambda (world)
     (begin
       (for-each (lambda (obj)
                   (let* ((bas (nth obj (base world)))
                          (tt (if (zero? bas) obj bas)))
                     ((nth (nth tt (prop world)) (nth tt (note world)))
                      #\newline I)))
                 (objects-here world))
       (goto get-user-input (increment-visits world)))))

(defmacro (commence-sub world p)
  (begin
    (#\newline I)
    (p #\newline I)
    (if (forced-move? (location world))
        (goto try-move world)
        (goto (if (dark world)
                  get-user-input
                  describe-objects)
              world))))

; 92 case TAKE:
(define-proc 'intransitive-take
  '(lambda (world)
     (let ((objs (objects-here world)))
       (if (and (pair? objs) (null? (cdr objs)))  ; TODO: check dwarf
           (goto transitive (set-obj world (K (car objs))))
           (goto get-object world)))))

; 92 case EAT:
(define-proc 'intransitive-eat
  '(lambda (world)
     (if (here? FOOD world)
         (goto transitive (set-obj world (K FOOD)))
         (goto get-object world))))


; 93 case OPEN: case CLOSE:
(define-proc 'intransitive-open
  '(lambda (world)
     (let ((object (cond ((or (= (nth GRATE (place world)) (location world))
                              (= (nth GRATE_ (place world)) (location world)))
                          GRATE)
                         ((= (nth DOOR (place world)) (location world)) DOOR)
                         ((here? CLAM world) CLAM)
                         ((here? OYSTER world) OYSTER)
                         (else V))))
       (if (here? CHAIN world)
           (if (object I I)
               (goto get-object world)
               (goto transitive (set-obj world (K CHAIN))))
           (if (object I I)
               (goto transitive (set-obj world (K object)))
               ((string "There is nothing here with a lock!\n")
                (goto get-user-input world)))))))

; 94 case INVENTORY:
(define-proc 'intransitive-inventory
  '(lambda (world)
     (let ((lst (objects-toting world)))
       (begin
         (if (null? lst)
             ((string "You're not carrying anything.\n") I)
             (begin
               ((string "You are currently holding the following:\n") I)
               (for-each (lambda (o)
                           (#\space (nth o objname) #\newline I))
                         lst)))
         (goto get-user-input world)))))

; 95 case BRIEF:
(define-proc 'intransitive-brief
  '(lambda (world)
     ((string "Okay, from now on I'll only describe a place in full the first time\nyou come to it.  To get the full description, say \"LOOK\".\n")
      (goto get-user-input (set-verbose world (K V))))))

; 98 case EAT:
(define-proc 'transitive-eat
  '(lambda (world)
     (if (= (obj world) FOOD)
         ((string "Thank you, it was delicious!\n")
          (goto get-user-input (destroy FOOD world)))
         (goto report-default world))))

; 102 case ON:
(define-proc 'transitive-on
  '(lambda (world)
     (if (not (here? LAMP world))
         (goto report-default world)
         (if (cons1? (limit world))
             (let-world ((set-nth world set-prop LAMP (K c1)))
               ((string "Your lamp is now on.\n")
                (goto (if (was-dark world)
                          commence
                          get-user-input) world)))
             ((string "Your lamp has run out of power.\n")
              (goto get-user-input world))))))

; 102 case OFF:
(define-proc 'transitive-off
  '(lambda (world)
     (if (not (here? LAMP world))
         (goto report-default world)
         (let-world ((set-nth world set-prop LAMP (K c0)))
           (begin
             ((string "Your lamp is now off.\n") I)
             ((dark world) pitch-dark-msg #\newline I)
             (goto get-user-input world))))))

; 112 case TAKE:
(define-proc 'transitive-take
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          ((toting? (obj world) world)  ; already carrying it
           ret (goto report-default world))
          ((nonzero? (nth (obj world) (base world)))  ; it is immovable
           (string "You can't be serious!\n")
           ret (goto get-user-input world))
         (let-world ((take-bird world ret)
		     (take-cage-bird world)
		     (carry (obj world) world))
           (begin
             ((string "OK.\n") I)
             (goto get-user-input world))))))))

(defmacro (take-cage-bird world)
  (cond ((= (obj world) BIRD)
         (carry CAGE world))
        ((and (= (obj world) CAGE) (nonzero? (nth BIRD (prop world))))
         (carry BIRD world))
        (else world)))

; 114 Check special cases for taking a bird
(defmacro (take-bird world ret)
  (if (and (= (obj world) BIRD) (zero? (nth BIRD (prop world))))
      (begin
        ((toting? ROD world)
         (string "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n")
         ret (goto get-user-input world))
        (if (toting? CAGE world)
            (set-nth world set-prop BIRD (K c1))
            ((string "You can catch the bird, but you cannot carry it.\n")
             ret (goto get-user-input world))))
      world))

; 120 Check special cases for dropping the bird
(defmacro (try-drop-bird world ret)
  (if (here? SNAKE world)
      (let-world ((destroy SNAKE world)
		  (set-nth world set-prop SNAKE (K c1))
		  (set-nth world set-prop BIRD (K c0)))
        ((string "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n")
         ret (goto get-user-input (drop BIRD (location world) world))))
      I))  ; TODO: handle dragon case

(defmacro (drop-cage-bird world)
  (cond ((= (obj world) BIRD)
         (set-nth world set-prop BIRD (K c0)))
        ((and (= (obj world) CAGE) (nonzero? (nth BIRD (prop world))))
         (drop BIRD (location world) world))
        (else world)))

; 117 case DROP:
(define-proc 'transitive-drop
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          ((not (toting? (obj world) world))
           ret (goto report-default world))
          ((= (obj world) BIRD)
           try-drop-bird world ret)
          (let-world ((drop-cage-bird world)
		      (drop (obj world) (location world) world))
            (begin
              ((string "OK.\n") I)
              (goto get-user-input world))))))))

(defmacro (open-close-grate world)
  ((nth (+ (nth GRATE (prop world)) (if (= (verb world) OPEN) c2 c0))
        (list (string "It was already locked.")
              (string "The grate is now locked.")
              (string "The grate is now unlocked.")
              (string "It was already unlocked.")))
   #\newline
   (set-nth world set-prop GRATE (K (if (= (verb world) OPEN) c1 c0)))))

; 130 case OPEN: calse CLOSE:
(define-proc 'transitive-open
  '(lambda (world)
     (if (= (obj world) GRATE)
         (if (here? KEYS world)
             (goto get-user-input (open-close-grate world))
             ((string "You have no keys!\n")
              (goto get-user-input world)))
         (goto report-default world))))

; 146 Determine the next location, newloc
(define-proc 'go-for-it
  '(lambda (world)
     (let ((q (find-inst (mot world)
                         (nth (location world) travels))))
       (if (not (pair? q))
           ((string "There is no way to ...\n")
            (goto mainloop world))
           (goto mainloop (apply-inst q world))))))

(define-proc 'quit
  '(lambda (world)
     ((string "\nquitting...\n") exit I)))

(add-unl-macro!
 'program-table '()
 `(list ,@(map (lambda (x) (compile-to-string (if (undefined? x) 'V x)))
               (reverse procedures))))

(defmacro initial-pc mainloop)
