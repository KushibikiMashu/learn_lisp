; 6.1
(defun say-hello()
  (print "Please type your name: ")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(print '3)	 	; 3 		integer
(print '3.4)	; 3.4 		Float
(print 'foo) 	; FOO 		symbol
(print '"foo") 	; "foo" 	string
(print '#\a)	; #\a 		literal

(princ '3) 		; 3
(princ '3.4) 	; 3.4
(princ 'foo) 	; FOO
(princ '"foo")	; foo
(princ '#\a)	; a

(defun say-hello()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

; 6.2
(defparameter *foo* '(+ 1 2))

(eval *foo*) ; 3

; 6.3
(defun game-repl()
  (loop (print (eval (read)))))

(defun game-repl()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
          (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

; * (game-repl)
; look
; You are in the living-room. A wizard is snoring loudly on the couch. There is
;  a door going west from here. There is a ladder going upstairs from here. You
;  see a whiskey on the floor. You see a bucket on the floor.
; walk west
; You are in a beautiful garden. There is a well in front of you. There is a
;  door going east from here. There is a nil going
;  (living-room downstairs ladder) from here. You see a frog on the floor. You
;  see a chain on the floor.

; pickup chain
; You are now carrying the chain

; scratch head
; I do not know that command.
; pickup chicken
; You cannot get that.

; walk east
; You are in the living-room. A wizard is snoring loudly on the couch. There is
;  a door going west from here. There is a ladder going upstairs from here. You
;  see a whiskey on the floor. You see a bucket on the floor.

; walk upstairs
; You are in the attic. There is a giant welding torch in the corner.

; inventory
; Item- chain

; walk china
; You cannot go that way.

; look
; You are in the attic. There is a giant welding torch in the corner.
; quit
; NIL
