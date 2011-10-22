#!/usr/bin/env gosh
(require "enum.scm")

(defmacro goto cons)

(define proc-labels '())
(define procedures '())

(define (define-proc name body)
  (set! proc-labels (cons name proc-labels))
  (set! procedures (cons body procedures)))

; 75 Simulate an adventure, going to quit when finished
(define-proc 'mainloop
  '(lambda (world)
     (let ((world2 (set-location world (lambda (_) (newloc world)))))
       (goto commence world2))))

; 75 try-move:
(define-proc 'try-move
  '(lambda (world)
     (let ((world1 (set-newloc world (lambda (_) (location world)))))
       ;; TODO: handle special motion words 140
       (goto go-for-it
             (set-oldlocs world1 (lambda (ol-ool)
                                   (cons (location world1) (car ol-ool))))))))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     (let* ((world2 (set-verb world (K ABSTAIN)))
	    (world3 (set-obj world2 (K NOTHING))))
       (goto cycle-label world3))))

; 76 cycle:
(define-proc 'cycle-label
  '(lambda (world)
     (let ((words listen))
       (goto look-at-word1 (set-word12 world (lambda (_) words))))))

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
               (print$ "object"
                       (goto quit world)))
             (lambda (_)  ; verb
	       (let ((world2 (set-verb world (K (word-meaning word1)))))
		 (if (word? word2)
		     (goto look-at-word1
			   (set-word12 world2 (K (icons word2 V))))
		     (goto (if (obj world2) transitive intransitive)
			   world2))))
             (lambda (_)  ; message
               (begin ((nth (word-meaning word1) (message world)) #\newline I)
                      (goto cycle-label world)))
             I)
            (goto cycle-label (unknown-word world)))))))

(define-proc 'intransitive
  '(lambda (world)
     (goto (nth (verb world)
                (list V
                      quit  ;TAKE
                      quit  ;DROP
                      quit  ;OPEN
                      quit  ;CLOSE
                      quit  ;ON
                      quit  ;OFF
                      quit  ;WAVE
                      quit  ;CALM
                      report-default  ;GO
                      report-default  ;RELAX
                      quit  ;POUR
                      quit  ;EAT
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
                      quit  ;BRIEF
                      quit  ;FIND
                      quit  ;INVENTORY
                      quit  ;SCORE
                      quit  ;QUIT
                 ))
           world)))

(define-proc 'transitive
  '(lambda (world)
     (goto (nth (verb world)
                (list V
                      quit  ;TAKE
                      quit  ;DROP
                      quit  ;OPEN
                      quit  ;CLOSE
                      quit  ;ON
                      quit  ;OFF
                      quit  ;WAVE
                      quit  ;CALM
                      quit  ;GO
                      quit  ;RELAX
                      quit  ;POUR
                      quit  ;EAT
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
                      quit  ;BRIEF
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

; 86 Report the current state
(define-proc 'commence
  '(lambda (world)
     (let ((selector (if (cons1? (nth (location world) (visits world)))
			 ->cdr
			 ->car)))
       (begin
         (#\newline I)
         ((nth (location world) room-desc) selector #\newline I)
         (goto describe-objects world)))))

(defmacro (increment-visits world)
  (set-visits
   world
   (modify-nth (to-cons1 (location world))
               (lambda (x)
                 (if (cons1? x)
                     (1-of-1 x)
                     (cons1 (cons1 (cons1 (cons1 V)))))))))

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
       (goto cycle-label (increment-visits world)))))

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

(define-enum (reverse proc-labels))

(add-unl-macro!
 'program-table '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V x))
               (reverse procedures))))

(defmacro initial-pc mainloop)
