(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (string &key (separator " "))
  (split-string-aux string separator))

(defun split-string-aux (string &optional (separator " ") (r nil))
  (let ((n (position separator string
                     :from-end t
                     :test #'(lambda (x y)
          (find y x :test #'string=)))))
    (if n
        (split-string-aux (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun get-fishes (input)
  (let ((line (car input)))
    (map 'list #'parse-integer (split-string line :separator ","))))

(defun count-fishes (timer days)
  (cond ((<= days 0) 1)
        ((= timer 0) (+ (count-fishes 6 (- days 1)) (count-fishes 8 (- days 1))))
        ((count-fishes (- timer 1) (- days 1)))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))

(setf (fdefinition 'count-fishes) (memoize #'count-fishes))

(defun fishes-after-days (fishes days)
  (reduce #'+ (map 'list #'(lambda (timer) (count-fishes timer days)) fishes)))

(let* ((input (get-file "inputs/06.input"))
       (fishes (get-fishes input))
       (part1 (fishes-after-days fishes 80))
       (part2 (fishes-after-days fishes 256)))
  (format t "Part 1: ~d~%Part 2: ~d~%" part1 part2))
