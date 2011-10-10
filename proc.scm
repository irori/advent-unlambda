#!/usr/bin/env gosh
(require "enum.scm")

;; labels
(define-enum
  '(get-user-input
    cycle-label
    look-at-word1
    report-the-current-state
    quit
    max-procedure
    ))

(defmacro initial-pc report-the-current-state)

(define procedures (make-vector (lookup-enum 'max-procedure)))

(define (define-proc name body)
  (vector-set! procedures (lookup-enum name) body))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     ((cons) cycle-label world)))

; 76 cycle:
(define-proc 'cycle-label
  '(lambda (world)
     (let ((words listen))
       ((cons) look-at-word1 (set-word12 world (lambda (_) words))))))

; 78 Look at word1 and exit to the right place if it completes a command
(define-proc 'look-at-word1
  '(lambda (world)
     ((word12 world)
      (lambda (word1 word2)
        (if (word? word1)
            (if (message-word? word1)
                (begin ((nth (word-meaning word1) (message world)) #\newline I)
                       ((cons) cycle-label world))
                (print$ "wtf??"
                        ((cons) quit world)))
            (print$ "I don't know that word.\n"  ; TODO: choose msg randomly
                    ((cons) cycle-label world)))))))

; 86 Report the current state
(define-proc 'report-the-current-state
  '(lambda (world)
     (begin
       ((nth (location world) (long-desc world)) #\newline I)
       ((cons) cycle-label world))))

(define-proc 'quit
  '(lambda (world)
     ((string "\nquitting...\n") exit I)))

(add-unl-macro!
 'program-table '()
 `(list ,@(map (lambda (x) (if (undefined? x) 'V x))
               (vector->list procedures))))
