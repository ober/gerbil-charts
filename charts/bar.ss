;;; -*- Gerbil -*-
;;; Bar chart
(export bar-chart render-bar-chart!)

(import :std/iter
        :gerbil-charts/canvas
        :gerbil-charts/canvas/svg
        :gerbil-charts/canvas/cairo
        :gerbil-charts/data
        :gerbil-charts/scale
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/legend)

;; High-level: create a complete bar chart
(def (bar-chart dataset
                width: (width 800) height: (height 400)
                title: (title #f)
                x-label: (x-label #f) y-label: (y-label #f)
                backend: (backend 'svg)
                filename: (filename #f))
  (let* ((canvas (case backend
                   ((svg) (make-svg-canvas width height))
                   ((png) (make-cairo-png-canvas width height filename))
                   ((pdf) (make-cairo-pdf-canvas width height filename))
                   (else (error "Unknown backend" backend))))
         (layout (make-default-layout width height title: title))
         (plot-area (layout-plot-area layout)))
    (render-bar-chart! canvas dataset layout plot-area
                       x-label: x-label y-label: y-label)
    {canvas.canvas-finish!}
    (case backend
      ((svg) {canvas.canvas-to-string})
      (else (void)))))

;; Core rendering
(def (render-bar-chart! canvas dataset layout plot-area
                        x-label: (x-label #f)
                        y-label: (y-label #f)
                        bar-width: (bar-width 0.8))
  (let-values (((x-min x-max) (dataset-x-range dataset))
               ((y-min y-max) (dataset-y-range dataset)))
    ;; Ensure Y range includes 0 for bar charts
    (let* ((y-min (min y-min 0))
           (y-pad (let ((d (- y-max y-min))) (if (zero? d) 1.0 (* 0.05 d))))
           (y-max (+ y-max y-pad))
           ;; Plot area bounds
           (px (car plot-area)) (py (cadr plot-area))
           (pw (caddr plot-area)) (ph (cadddr plot-area))
           ;; Scales
           (x-scale (make-linear-scale x-min x-max px (+ px pw)))
           (y-scale (make-linear-scale y-min y-max (+ py ph) py))
           ;; Bar geometry
           (n-series (length (dataset-series dataset)))
           (n-points (length (data-series-points (car (dataset-series dataset)))))
           (group-width (/ pw n-points))
           (bar-w (/ (* group-width bar-width) n-series))
           (baseline-py (scale-map y-scale 0)))

      ;; Draw layout
      (draw-layout! canvas layout)

      ;; Draw grid
      (draw-grid! canvas x-scale y-scale plot-area)

      ;; Draw bars
      (let si-loop ((series-rest (dataset-series dataset)) (si 0))
        (when (pair? series-rest)
          (let ((series (car series-rest))
                (color (data-series-color (car series-rest))))
            {canvas.canvas-set-color! color}
            (let pi-loop ((pts (data-series-points series)) (pi 0))
              (when (pair? pts)
                (let* ((pt (car pts))
                       (group-x (+ px (* pi group-width)
                                   (* 0.5 (- group-width (* bar-w n-series)))))
                       (bar-x (+ group-x (* si bar-w)))
                       (bar-y (scale-map y-scale (data-point-y pt)))
                       (bar-h (- baseline-py bar-y)))
                  {canvas.canvas-draw-rect! bar-x bar-y bar-w bar-h #t}
                  (pi-loop (cdr pts) (+ pi 1)))))
            (si-loop (cdr series-rest) (+ si 1)))))

      ;; Draw axes
      {canvas.canvas-set-color! [0.0 0.0 0.0]}
      {canvas.canvas-set-line-width! 1}
      (draw-x-axis! canvas x-scale (+ py ph) px (+ px pw) label: x-label)
      (draw-y-axis! canvas y-scale px py (+ py ph) label: y-label)

      ;; Draw legend
      (when (> n-series 1)
        (draw-legend! canvas (dataset-series dataset) plot-area)))))
