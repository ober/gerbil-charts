;;; -*- Gerbil -*-
;;; Legend rendering
(export draw-legend!)

(import :gerbil-charts/canvas
        :gerbil-charts/data)

;; Draw a legend box in the top-right of the plot area
(def (draw-legend! canvas series-list plot-area)
  (let* ((x (+ (car plot-area) (caddr plot-area) -120))
         (y (+ (cadr plot-area) 10))
         (line-height 18))
    (let loop ((rest series-list) (i 0))
      (when (pair? rest)
        (let ((s (car rest))
              (sy (+ y (* i line-height))))
          (let ((color (data-series-color s)))
            ;; Color swatch
            {canvas.canvas-set-color! color}
            {canvas.canvas-draw-rect! x sy 12 12 #t}
            ;; Label
            {canvas.canvas-set-color! [0.0 0.0 0.0]}
            {canvas.canvas-set-font! "sans-serif" 11 "normal"}
            {canvas.canvas-draw-text! (+ x 16) (+ sy 10) (data-series-name s) "start" "auto"})
          (loop (cdr rest) (+ i 1)))))))
