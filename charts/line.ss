;;; -*- Gerbil -*-
;;; Line chart
(export line-chart render-line-chart!)

(import :std/iter
        :std/sort
        :gerbil-charts/canvas
        :gerbil-charts/canvas/svg
        :gerbil-charts/canvas/cairo
        :gerbil-charts/data
        :gerbil-charts/scale
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/legend)

;; High-level: create a complete line chart
(def (line-chart dataset
                 width: (width 800) height: (height 400)
                 title: (title #f)
                 x-label: (x-label #f) y-label: (y-label #f)
                 backend: (backend 'svg)
                 filename: (filename #f))
  (let* ((canvas (case backend
                   ((svg) (make-svg-canvas width height))
                   ((png) (make-cairo-png-canvas width height filename))
                   ((pdf) (make-cairo-pdf-canvas width height filename))
                   ((cairo-svg) (make-cairo-svg-canvas width height filename))
                   (else (error "Unknown backend" backend))))
         (layout (make-default-layout width height title: title))
         (plot-area (layout-plot-area layout)))
    (render-line-chart! canvas dataset layout plot-area
                        x-label: x-label y-label: y-label)
    {canvas.canvas-finish!}
    (when (and (eq? backend 'png) filename)
      (void))  ;; finish! already writes PNG
    (case backend
      ((svg) {canvas.canvas-to-string})
      (else (void)))))

;; Core rendering (reusable with any canvas)
(def (render-line-chart! canvas dataset layout plot-area
                         x-label: (x-label #f)
                         y-label: (y-label #f))
  (let-values (((x-min x-max) (dataset-x-range dataset))
               ((y-min y-max) (dataset-y-range dataset)))
    ;; Add 5% padding to Y range
    (let* ((y-pad (let ((d (- y-max y-min))) (if (zero? d) 1.0 (* 0.05 d))))
           (y-min (- y-min y-pad))
           (y-max (+ y-max y-pad))
           ;; Plot area bounds
           (px (car plot-area)) (py (cadr plot-area))
           (pw (caddr plot-area)) (ph (cadddr plot-area))
           ;; Scales (Y inverted: data up = pixels down)
           (x-scale (make-linear-scale x-min x-max px (+ px pw)))
           (y-scale (make-linear-scale y-min y-max (+ py ph) py)))

      ;; Draw layout (background, title)
      (draw-layout! canvas layout)

      ;; Draw grid
      (draw-grid! canvas x-scale y-scale plot-area)

      ;; Draw axes
      {canvas.canvas-set-color! [0.0 0.0 0.0]}
      {canvas.canvas-set-line-width! 1}
      (draw-x-axis! canvas x-scale (+ py ph) px (+ px pw) label: x-label)
      (draw-y-axis! canvas y-scale px py (+ py ph) label: y-label)

      ;; Draw each series
      (for (series (dataset-series dataset))
        (let ((color (data-series-color series))
              (points (data-series-points series)))
          {canvas.canvas-set-color! color}
          {canvas.canvas-set-line-width! 2}
          ;; Sort points by X and draw connected line segments
          (let ((sorted (sort points (lambda (a b) (< (data-point-x a) (data-point-x b))))))
            (when (and (pair? sorted) (pair? (cdr sorted)))
              (let loop ((prev (car sorted)) (rest (cdr sorted)))
                (when (pair? rest)
                  (let* ((cur (car rest))
                         (px1 (scale-map x-scale (data-point-x prev)))
                         (py1 (scale-map y-scale (data-point-y prev)))
                         (px2 (scale-map x-scale (data-point-x cur)))
                         (py2 (scale-map y-scale (data-point-y cur))))
                    {canvas.canvas-draw-line! px1 py1 px2 py2}
                    (loop cur (cdr rest)))))))))

      ;; Draw legend
      (when (> (length (dataset-series dataset)) 1)
        (draw-legend! canvas (dataset-series dataset) plot-area)))))
