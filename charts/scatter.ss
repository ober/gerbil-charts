;;; -*- Gerbil -*-
;;; Scatter plot
(export scatter-chart render-scatter-chart!)

(import :std/iter
        :gerbil-charts/canvas
        :gerbil-charts/canvas/svg
        :gerbil-charts/canvas/cairo
        :gerbil-charts/data
        :gerbil-charts/scale
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/legend)

;; High-level: create a complete scatter chart
(def (scatter-chart dataset
                    width: (width 800) height: (height 400)
                    title: (title #f)
                    x-label: (x-label #f) y-label: (y-label #f)
                    point-radius: (point-radius 4)
                    backend: (backend 'svg)
                    filename: (filename #f))
  (let* ((canvas (case backend
                   ((svg) (make-svg-canvas width height))
                   ((png) (make-cairo-png-canvas width height filename))
                   ((pdf) (make-cairo-pdf-canvas width height filename))
                   (else (error "Unknown backend" backend))))
         (layout (make-default-layout width height title: title))
         (plot-area (layout-plot-area layout)))
    (render-scatter-chart! canvas dataset layout plot-area
                           x-label: x-label y-label: y-label
                           point-radius: point-radius)
    {canvas.canvas-finish!}
    (case backend
      ((svg) {canvas.canvas-to-string})
      (else (void)))))

;; Core rendering
(def (render-scatter-chart! canvas dataset layout plot-area
                            x-label: (x-label #f)
                            y-label: (y-label #f)
                            point-radius: (point-radius 4))
  (let-values (((x-min x-max) (dataset-x-range dataset))
               ((y-min y-max) (dataset-y-range dataset)))
    (let* ((x-pad (let ((d (- x-max x-min))) (if (zero? d) 1.0 (* 0.05 d))))
           (y-pad (let ((d (- y-max y-min))) (if (zero? d) 1.0 (* 0.05 d))))
           (x-min (- x-min x-pad)) (x-max (+ x-max x-pad))
           (y-min (- y-min y-pad)) (y-max (+ y-max y-pad))
           ;; Plot area bounds
           (px (car plot-area)) (py (cadr plot-area))
           (pw (caddr plot-area)) (ph (cadddr plot-area))
           ;; Scales
           (x-scale (make-linear-scale x-min x-max px (+ px pw)))
           (y-scale (make-linear-scale y-min y-max (+ py ph) py)))

      ;; Draw layout
      (draw-layout! canvas layout)

      ;; Draw grid
      (draw-grid! canvas x-scale y-scale plot-area)

      ;; Draw axes
      {canvas.canvas-set-color! [0.0 0.0 0.0]}
      {canvas.canvas-set-line-width! 1}
      (draw-x-axis! canvas x-scale (+ py ph) px (+ px pw) label: x-label)
      (draw-y-axis! canvas y-scale px py (+ py ph) label: y-label)

      ;; Draw points
      (for (series (dataset-series dataset))
        (let ((color (data-series-color series)))
          {canvas.canvas-set-color! color}
          (for (pt (data-series-points series))
            (let ((spx (scale-map x-scale (data-point-x pt)))
                  (spy (scale-map y-scale (data-point-y pt))))
              {canvas.canvas-draw-circle! spx spy point-radius #t}))))

      ;; Draw legend
      (when (> (length (dataset-series dataset)) 1)
        (draw-legend! canvas (dataset-series dataset) plot-area)))))
