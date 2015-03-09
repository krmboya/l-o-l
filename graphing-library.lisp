;; substitutes non-alphanumeric characters in node names to give valid DOT names
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
