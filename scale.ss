;;; -*- Gerbil -*-
;;; Scales: map data coordinates to pixel coordinates
(export
  make-linear-scale linear-scale?
  linear-scale-domain-min linear-scale-domain-max
  linear-scale-range-min linear-scale-range-max
  scale-map
  scale-inverse
  scale-ticks)

(defstruct linear-scale (domain-min domain-max range-min range-max)
  transparent: #t)

;; Map a data value to pixel position
(def (scale-map scale value)
  (let ((d-span (- (linear-scale-domain-max scale) (linear-scale-domain-min scale)))
        (r-span (- (linear-scale-range-max scale) (linear-scale-range-min scale))))
    (if (zero? d-span) (linear-scale-range-min scale)
      (+ (linear-scale-range-min scale)
         (* (/ (- value (linear-scale-domain-min scale)) d-span) r-span)))))

;; Inverse mapping: pixel -> data value
(def (scale-inverse scale pixel)
  (let ((d-span (- (linear-scale-domain-max scale) (linear-scale-domain-min scale)))
        (r-span (- (linear-scale-range-max scale) (linear-scale-range-min scale))))
    (if (zero? r-span) (linear-scale-domain-min scale)
      (+ (linear-scale-domain-min scale)
         (* (/ (- pixel (linear-scale-range-min scale)) r-span) d-span)))))

;; Generate "nice" tick values for an axis
;; Returns a list of data-space values
(def (scale-ticks scale (target-count 5))
  (let* ((d-min (linear-scale-domain-min scale))
         (d-max (linear-scale-domain-max scale))
         (range (- d-max d-min)))
    (if (zero? range)
      (list d-min)
      (let* ((rough-step (/ range target-count))
             (magnitude (expt 10 (floor (/ (log (exact->inexact rough-step)) (log 10.0)))))
             (nice-step
               (let ((norm (/ rough-step magnitude)))
                 (cond ((< norm 1.5) (* 1 magnitude))
                       ((< norm 3.5) (* 2 magnitude))
                       ((< norm 7.5) (* 5 magnitude))
                       (else (* 10 magnitude)))))
             (tick-min (* (ceiling (/ d-min nice-step)) nice-step))
             (tick-max (* (floor (/ d-max nice-step)) nice-step)))
        (let loop ((v tick-min) (ticks []))
          (if (> v (+ tick-max (* 0.5 nice-step)))
            (reverse ticks)
            (loop (+ v nice-step) (cons v ticks))))))))
