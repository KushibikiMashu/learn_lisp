;; fizzbuzz
(defun fizzbuzz (n)
    (if (or (zerop (mod n 3))
                    (zerop (mod n 5)))
              (progn
                        (when (and (zerop (mod n 3)))
                                  (format t "fizz"))
                                (when (and (zerop (mod n 5)))
                                          (format t "fizz"))
                                          (format t " "))
                (format t "~a " n)))
