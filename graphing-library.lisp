;; substitutes non-alphanumeric characters in node names to give valid DOT names
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

;; generate a label of *max-label-length* that should appear on a graph node
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

;; generate dot info from nodes
(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"]"))
	nodes))

;; generate dot info from edges
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"]"))
		(cdr node)))
	edges))

;; generate all dot data
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; turn the dot file into a picture
;; requires the graphviz package with the executable `/usr/bin/dot`
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/usr/bin/dot"  `("-Tpng" "-O"  ,fname) :output *standard-output*))

;; turn whole graph into a picture
;; fname is a full path to the output file
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))