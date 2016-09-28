(defparameter *num-players* 2)
(defparameter *max-dice* 3)  ;; dice per square
(defparameter *board-size* 2) ;; 2 by 2
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  "Returns an array of board represention from list `lst`"
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  "Generates a random board"
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))
(defun player-letter (n)
  "Ascii representation of player"
  (code-char (+ 97 n)))
