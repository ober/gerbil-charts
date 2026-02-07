;;; -*- Gerbil -*-
;;; Area chart (filled line chart)
(export area-chart render-area-chart!)

(import :std/iter
        :std/sort
        :std/misc/list
        :gerbil-charts/canvas
        :gerbil-charts/canvas/svg
        :gerbil-charts/canvas/cairo
        :gerbil-charts/data
        :gerbil-charts/scale
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/legend)

;; High-level: create a complete area chart
(def (area-chart dataset
                 width: (width 800) height: (height 400)
                 title: (title #f)
                 x-label: (x-label #f) y-label: (y-label #f)
                 opacity: (opacity 0.3)
                 backend: (backend 'svg)
                 filename: (filename #f))
  (let* ((canvas (case backend
                   ((svg) (make-svg-canvas width height))
                   ((png) (make-cairo-png-canvas width height filename))
                   ((pdf) (make-cairo-pdf-canvas width height filename))
                   (else (error "Unknown backend" backend))))
         (layout (make-default-layout width height title: title))
         (plot-area (layout-plot-area layout)))
    (render-area-chart! canvas dataset layout plot-area
                        x-label: x-label y-label: y-label
                        opacity: opacity)
    {canvas.canvas-finish!}
    (case backend
      ((svg) {canvas.canvas-to-string})
      (else (void)))))

;; Core rendering
(def (render-area-chart! canvas dataset layout plot-area
                         x-label: (x-label #f)
                         y-label: (y-label #f)
                         opacity: (opacity 0.3))
  (let-values (((x-min x-max) (dataset-x-range dataset))
               ((y-min y-max) (dataset-y-range dataset)))
    (let* ((y-pad (let ((d (- y-max y-min))) (if (zero? d) 1.0 (* 0.05 d))))
           (y-min (- y-min y-pad))
           (y-max (+ y-max y-pad))
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

      ;; Draw filled area + line for each series
      (for (series (dataset-series dataset))
        (let* ((color (data-series-color series))
               (sorted (sort (data-series-points series)
                             (lambda (a b) (< (data-point-x a) (data-point-x b)))))
               ;; Build line points in pixel space
               (line-pts (map (lambda (pt)
                                (list (scale-map x-scale (data-point-x pt))
                                      (scale-map y-scale (data-point-y pt))))
                              sorted))
               ;; Baseline Y (bottom of domain)
               (baseline-y (scale-map y-scale y-min))
               (first-x (car (car line-pts)))
               (last-x (car (last line-pts)))
               ;; Polygon: line points + baseline return
               (polygon (append line-pts
                                (list (list last-x baseline-y)
                                      (list first-x baseline-y)))))
          ;; Fill with transparency
          {canvas.canvas-set-color! (append color (list opacity))}
          {canvas.canvas-draw-path! polygon #t}
          ;; Stroke the line on top
          {canvas.canvas-set-color! color}
          {canvas.canvas-set-line-width! 2}
          (when (and (pair? sorted) (pair? (cdr sorted)))
            (let loop ((prev (car sorted)) (rest (cdr sorted)))
              (when (pair? rest)
                (let* ((cur (car rest))
                       (px1 (scale-map x-scale (data-point-x prev)))
                       (py1 (scale-map y-scale (data-point-y prev)))
                       (px2 (scale-map x-scale (data-point-x cur)))
                       (py2 (scale-map y-scale (data-point-y cur))))
                  {canvas.canvas-draw-line! px1 py1 px2 py2}
                  (loop cur (cdr rest))))))))

      ;; Draw axes
      {canvas.canvas-set-color! [0.0 0.0 0.0]}
      {canvas.canvas-set-line-width! 1}
      (draw-x-axis! canvas x-scale (+ py ph) px (+ px pw) label: x-label)
      (draw-y-axis! canvas y-scale px py (+ py ph) label: y-label)

      ;; Draw legend
      (when (> (length (dataset-series dataset)) 1)
        (draw-legend! canvas (dataset-series dataset) plot-area)))))
