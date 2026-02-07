;;; -*- Gerbil -*-
(import :std/test :gerbil-charts/data)
(export data-test)

(def data-test
  (test-suite "data model"
    (test-case "data-point creation"
      (let ((p (make-data-point 1.0 2.5 "label")))
        (check (data-point-x p) => 1.0)
        (check (data-point-y p) => 2.5)
        (check (data-point-label p) => "label")))

    (test-case "series range"
      (let ((s (make-data-series "test"
                [(make-data-point 1 10 #f)
                 (make-data-point 5 3 #f)
                 (make-data-point 3 7 #f)]
                [0 0 1] 'solid)))
        (let-values (((lo hi) (series-x-range s)))
          (check lo => 1)
          (check hi => 5))
        (let-values (((lo hi) (series-y-range s)))
          (check lo => 3)
          (check hi => 10))))

    (test-case "dataset range"
      (let* ((s1 (make-data-series "a"
                   [(make-data-point 0 5 #f) (make-data-point 10 15 #f)]
                   [1 0 0] 'solid))
             (s2 (make-data-series "b"
                   [(make-data-point 2 1 #f) (make-data-point 8 20 #f)]
                   [0 1 0] 'solid))
             (ds (make-dataset [s1 s2] "test")))
        (let-values (((lo hi) (dataset-x-range ds)))
          (check (= lo 0) ? values)
          (check (= hi 10) ? values))
        (let-values (((lo hi) (dataset-y-range ds)))
          (check (= lo 1) ? values)
          (check (= hi 20) ? values))))

    (test-case "downsample no-op when below max"
      (let* ((pts [(make-data-point 1 10 #f)
                   (make-data-point 2 20 #f)
                   (make-data-point 3 30 #f)])
             (s (make-data-series "test" pts [1 0 0] 'solid))
             (result (downsample s 5)))
        ;; Should return same series when points <= max
        (check (eq? result s) ? values)))

    (test-case "downsample reduces points"
      (let* ((pts (let loop ((i 0) (acc []))
                    (if (>= i 100)
                      (reverse acc)
                      (loop (+ i 1) (cons (make-data-point i (* i 2) #f) acc)))))
             (s (make-data-series "test" pts [1 0 0] 'solid))
             (result (downsample s 10)))
        ;; Should have exactly 10 points
        (check (= (length (data-series-points result)) 10) ? values)
        ;; First point should be preserved
        (check (data-point-x (car (data-series-points result))) => 0)
        ;; Last point should be preserved
        (let ((last-pt (car (reverse (data-series-points result)))))
          (check (data-point-x last-pt) => 99))
        ;; Metadata should be preserved
        (check (data-series-name result) => "test")
        (check (data-series-color result) => [1 0 0])))))
