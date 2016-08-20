; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; version 2 of the License.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; Partial Author: Conrad Barski, M.D.
; Parts Adapted with permission from http.lisp by Ron Garret

(defun decode-param (s)
  "Recursively decodes a URL encoded string"
  (labels ((f (lst)
	     (when lst
	       ;; check the first character
	       (case (car lst)

		 ;; if % convert following hex number to a character i.e.
		 ;; next two characters
		 ;; if invalid, returns nil (junk-allowed)
		 (#\% (cons (code-char (parse-integer (coerce (list 
							       (cadr lst) 
							       (caddr lst)) 
							      'string) 
						      :radix 16 
						      :junk-allowed t))
			    (f (cdddr lst))))
		 ;; if + replace with space
		 (#\+ (cons #\space (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    ;; convert string to list, process it, then back to string
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  "Returns list of conses each representing a key value pair from query params"
   (let* ((i1 (position #\= s)) ;; pos of =
          (i2 (position #\& s))) ;; pos of &
      (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                            (decode-param (subseq s (1+ i1) i2)))
		      ;; recursively parse rest of string generating conses
		      ;; or evaluate to nil
                      (and i2 (parse-params (subseq s (1+ i2))))))
            ((equal s "") nil) ;; if string is blank return nil
            (t s))))  ;; otherwise return the string itself
