;;; -*- Gerbil -*-
;;; Axis rendering: ticks, labels, grid lines
(export
  draw-x-axis!
  draw-y-axis!
  draw-grid!
  make-time-format-fn
  make-metric-format-fn)

(import :std/iter
        :std/format
        :std/srfi/19
        :gerbil-charts/canvas
        :gerbil-charts/scale)

;; Draw X axis with ticks and labels
(def (draw-x-axis! canvas scale
                   y-position           ;; pixel Y where axis sits
                   x-start x-end        ;; pixel range
                   label: (label #f)
                   tick-length: (tick-length 5)
                   format-fn: (format-fn number->string))
  ;; Draw axis line
  {canvas.canvas-draw-line! x-start y-position x-end y-position}
  ;; Draw ticks and labels
  (for (tick-val (scale-ticks scale))
    (let ((px (scale-map scale tick-val)))
      ;; Tick mark
      {canvas.canvas-draw-line! px y-position px (+ y-position tick-length)}
      ;; Label
      {canvas.canvas-set-font! "sans-serif" 10 "normal"}
      {canvas.canvas-draw-text! px (+ y-position tick-length 12)
        (format-fn tick-val) "middle" "auto"}))
  ;; Axis label
  (when label
    (let ((mid (/ (+ x-start x-end) 2)))
      {canvas.canvas-set-font! "sans-serif" 12 "normal"}
      {canvas.canvas-draw-text! mid (+ y-position tick-length 30)
        label "middle" "auto"})))

;; Draw Y axis with ticks and labels
(def (draw-y-axis! canvas scale
                   x-position
                   y-start y-end
                   label: (label #f)
                   tick-length: (tick-length 5)
                   format-fn: (format-fn number->string))
  ;; Draw axis line
  {canvas.canvas-draw-line! x-position y-start x-position y-end}
  ;; Draw ticks and labels
  (for (tick-val (scale-ticks scale))
    (let ((py (scale-map scale tick-val)))
      ;; Tick mark
      {canvas.canvas-draw-line! x-position py (- x-position tick-length) py}
      ;; Label
      {canvas.canvas-set-font! "sans-serif" 10 "normal"}
      {canvas.canvas-draw-text! (- x-position tick-length 5) py
        (format-fn tick-val) "end" "middle"}))
  ;; Axis label
  (when label
    {canvas.canvas-set-font! "sans-serif" 12 "normal"}
    {canvas.canvas-draw-text! (- x-position 50) (/ (+ y-start y-end) 2)
      label "middle" "middle"}))

;; Draw grid lines
(def (draw-grid! canvas x-scale y-scale plot-area)
  (let ((x (car plot-area)) (y (cadr plot-area))
        (w (caddr plot-area)) (h (cadddr plot-area)))
    ;; Light gray grid
    {canvas.canvas-save!}
    {canvas.canvas-set-color! [0.85 0.85 0.85]}
    {canvas.canvas-set-line-width! 0.5}
    ;; Vertical grid lines from X ticks
    (for (tick-val (scale-ticks x-scale))
      (let ((px (scale-map x-scale tick-val)))
        {canvas.canvas-draw-line! px y px (+ y h)}))
    ;; Horizontal grid lines from Y ticks
    (for (tick-val (scale-ticks y-scale))
      (let ((py (scale-map y-scale tick-val)))
        {canvas.canvas-draw-line! x py (+ x w) py}))
    {canvas.canvas-restore!}))

;; Return a formatting function for X-axis time labels based on the data range
;; range-seconds: total time span of the data in seconds
;; Adapts format granularity: hours for intraday, dates for multi-day, etc.
(def (make-time-format-fn range-seconds)
  (let ((fmt (cond
               ((>= range-seconds (* 365 86400)) "~b ~d")    ;; >= 1 year: "Jan 01"
               ((>= range-seconds (* 30 86400))  "~b ~d")    ;; >= 30 days: "Jan 02"
               ((>= range-seconds (* 7 86400))   "~m/~d")    ;; >= 7 days: "01/02"
               ((>= range-seconds 86400)          "~m/~d ~H:~M") ;; >= 1 day
               (else                              "~H:~M"))))  ;; < 1 day
    (lambda (unix-ts)
      (let* ((secs (inexact->exact (floor unix-ts)))
             (t (make-time time-utc 0 secs))
             (d (time-utc->date t 0)))
        (date->string d fmt)))))

;; Return a formatting function for Y-axis metric values.
;; Formats large values with K/M suffixes, small with scientific notation.
(def (make-metric-format-fn)
  (lambda (val)
    (let ((v (exact->inexact val)))
      (cond
        ((zero? v) "0")
        ((or (>= (abs v) 1e6) (< (abs v) 0.01))
         (format "~ae~a"
                 (let ((m (/ v (expt 10.0 (floor (/ (log (abs v)) (log 10.0)))))))
                   (/ (floor (* m 10.0)) 10.0))
                 (inexact->exact (floor (/ (log (abs v)) (log 10.0))))))
        ((>= (abs v) 1000.0)
         (format "~aK" (/ (floor (* (/ v 1000.0) 10.0)) 10.0)))
        (else
         (let ((r (/ (floor (* v 100.0)) 100.0)))
           (if (= r (floor r))
             (number->string (inexact->exact (floor r)))
             (number->string r))))))))
