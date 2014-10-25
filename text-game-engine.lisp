(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

;; e.g. (describe-location 'living-room *nodes*) ->
;;      (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; e.g. (describe-path '(garden west door)) ->
;;      (THERE IS A DOOR GOING WEST FROM HERE.)
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) ;; quasi-quoting

;; Retrieve edges associated with location
;; Create a description of each edge
;; Join the descriptions
;; #' foo is expanded to (function foo)
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

