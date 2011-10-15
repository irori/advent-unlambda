#!/usr/bin/env gosh
(require "enum.scm")

;; labels
(define-enum
  '(mainloop
    try-move
    get-user-input
    cycle-label
    look-at-word1
    commence
    go-for-it
    quit
    max-procedure
    ))

(defmacro initial-pc mainloop)

(defmacro goto (cons))

(define procedures (make-vector (lookup-enum 'max-procedure)))

(define (define-proc name body)
  (vector-set! procedures (lookup-enum name) body))

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
             (set-oldlocs world1 (lambda (p)
                                   (p (lambda (ol ool)
                                        (cons (location world1) ol)))))))))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     (goto cycle-label world)))

; 76 cycle:
(define-proc 'cycle-label
  '(lambda (world)
     (let ((words listen))
       (goto look-at-word1 (set-word12 world (lambda (_) words))))))

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
               (print$ "verb"
                       (goto quit world)))
             (lambda (_)  ; message
               (begin ((nth (word-meaning word1) (message world)) #\newline I)
                      (goto cycle-label world)))
             I)
            (print$ "I don't know that word.\n"  ; TODO: choose msg randomly
                    (goto cycle-label world)))))))

; 86 Report the current state
(define-proc 'commence
  '(lambda (world)
     (begin
       ((nth (location world) (long-desc world)) #\newline I)
       (goto cycle-label world))))

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
 `(list ,@(map (lambda (x) (if (undefined? x) 'V x))
               (vector->list procedures))))
