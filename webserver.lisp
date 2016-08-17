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
   (labels ((f (lst)
               (when lst
                 (case (car lst)
                     (#\% (cons (code-char (parse-integer (coerce (list (cadr lst) (caddr lst)) 'string) :radix 16 :junk-allowed t))
                                (f (cdddr lst))))
                     (#\+ (cons #\space (f (cdr lst))))
                     (otherwise (cons (car lst) (f (cdr lst))))))))
       (coerce (f (coerce s 'list)) 'string)))
