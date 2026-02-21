;;; -*- Gerbil -*-
(import :std/test :std/iter :gerbil-charts/scale)
(export scale-test)

(def scale-test
  (test-suite "scales"
    (test-case "linear scale mapping"
      (let ((s (make-linear-scale 0 100 0 500)))
        (check (scale-map s 0) => 0)
        (check (scale-map s 100) => 500)
        (check (scale-map s 50) => 250)))

    (test-case "scale inverse"
      (let ((s (make-linear-scale 0 100 0 500)))
        (check (scale-inverse s 250) => 50)))

    (test-case "tick generation"
      (let* ((s (make-linear-scale 0 100 0 500))
             (ticks (scale-ticks s 5)))
        ;; Should have reasonable number of ticks
        (check (> (length ticks) 2) ? values)
        (check (< (length ticks) 12) ? values)
        ;; All ticks should be within domain
        (for (t ticks)
          (check (>= t 0) ? values)
          (check (<= t 100) ? values))))

    (test-case "zero-range scale"
      (let ((s (make-linear-scale 5 5 0 500)))
        (check (scale-map s 5) => 0)
        (check (length (scale-ticks s)) => 1)))))
