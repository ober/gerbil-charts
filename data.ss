;;; -*- Gerbil -*-
;;; Data model: points, series, datasets
(export
  make-data-point data-point? data-point-x data-point-y data-point-label
  make-data-series data-series?
  data-series-name data-series-points data-series-color data-series-style
  make-dataset dataset? dataset-series dataset-title
  series-x-range series-y-range
  dataset-x-range dataset-y-range
  downsample)

(defstruct data-point (x y label)
  transparent: #t)

(defstruct data-series (name points color style)
  transparent: #t)
;; color: [r g b] floats 0-1
;; style: 'solid | 'dashed | 'dotted (for lines)
;;        'filled | 'outlined (for bars/scatter)

(defstruct dataset (series title)
  transparent: #t)

;; Compute X range of a series
(def (series-x-range series)
  (let ((xs (map data-point-x (data-series-points series))))
    (values (apply min xs) (apply max xs))))

;; Compute Y range of a series
(def (series-y-range series)
  (let ((ys (map data-point-y (data-series-points series))))
    (values (apply min ys) (apply max ys))))

;; Compute X range across all series in a dataset
(def (dataset-x-range ds)
  (let loop ((series (dataset-series ds)) (lo +inf.0) (hi -inf.0))
    (if (null? series) (values lo hi)
      (let-values (((slo shi) (series-x-range (car series))))
        (loop (cdr series) (min lo slo) (max hi shi))))))

;; Compute Y range across all series in a dataset
(def (dataset-y-range ds)
  (let loop ((series (dataset-series ds)) (lo +inf.0) (hi -inf.0))
    (if (null? series) (values lo hi)
      (let-values (((slo shi) (series-y-range (car series))))
        (loop (cdr series) (min lo slo) (max hi shi))))))

;; Reduce a data series to at most max-points using stride-based sampling.
;; Preserves first and last points.
(def (downsample series max-points)
  (let* ((pts (data-series-points series))
         (n (length pts))
         (vec (list->vector pts)))
    (if (<= n max-points)
      series
      (let* ((stride (/ (- n 1.0) (- max-points 1.0)))
             (sampled
               (let loop ((i 0) (acc []))
                 (if (>= i max-points)
                   (reverse acc)
                   (let ((idx (inexact->exact (floor (* i stride)))))
                     (loop (+ i 1) (cons (vector-ref vec (min idx (- n 1))) acc)))))))
        (make-data-series
          (data-series-name series)
          sampled
          (data-series-color series)
          (data-series-style series))))))
