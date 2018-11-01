;; 5.1
;; define location and get to know where you are.
(defparameter *nodes* '((living-room (you are in the living-room.
							a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     there is a well in front of you.))
                         (attic (you are in the attic.
                                     there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; 5.2
;; define path
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door)
                        (attic (living-room downstairs ladder)))))

;; how to unquote to use code mode in a data mode
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; usage of mapcar
(mapcar #'sqrt '(1 2 3 4 5)) ;; (1.0 1.4142135 1.7320508 2.0 2.236068)
(mapcar #'car '((foo bar) (baz qux))) ;; (FOO BAZ)

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; 5.3
;; objects
(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
         	(eq (cadr (assoc obj obj-locs)) loc)))
      (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
                 `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

;; 5.4
(defparameter *location* 'living-room)

(defun look()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; * (look)
;; (YOU ARE IN THE LIVING-ROOM.
;; A WIZARD IS SNORING LOUDLY ON THE COUCH.
;; THERE IS A DOOR GOING WEST FROM HERE.
;; THERE IS A LADDER GOING UPSTAIRS FROM HERE.
;; YOU SEE A WHISKEY ON THE FLOOR.
;; YOU SEE A BUCKET ON THE FLOOR.)

;; 5.5
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

;; 5.6
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

;; 5.7
(defun inventory()
  (cons 'item- (objects-at 'body *objects* *object-locations*)))
