;;; -*- Gerbil -*-
;;; Chart layout: margins, title, plot area computation
(export
  make-chart-layout chart-layout?
  chart-layout-width chart-layout-height
  chart-layout-margin-top chart-layout-margin-right
  chart-layout-margin-bottom chart-layout-margin-left
  chart-layout-title
  make-default-layout
  layout-plot-area
  draw-layout!
  truncate-title)

(import :gerbil-charts/canvas)

(defstruct chart-layout
  (width height
   margin-top margin-right margin-bottom margin-left
   title)
  transparent: #t)

(def (make-default-layout width height
                          title: (title #f)
                          margin-top: (margin-top 40)
                          margin-right: (margin-right 30)
                          margin-bottom: (margin-bottom 60)
                          margin-left: (margin-left 70))
  (make-chart-layout width height margin-top margin-right margin-bottom margin-left title))

;; Returns (x y w h) of the plot area
(def (layout-plot-area layout)
  (list (chart-layout-margin-left layout)
        (chart-layout-margin-top layout)
        (- (chart-layout-width layout)
           (chart-layout-margin-left layout)
           (chart-layout-margin-right layout))
        (- (chart-layout-height layout)
           (chart-layout-margin-top layout)
           (chart-layout-margin-bottom layout))))

;; Truncate a string to max-len characters, adding "..." if truncated
(def (truncate-title str (max-len 60))
  (if (<= (string-length str) max-len)
    str
    (string-append (substring str 0 (- max-len 3)) "...")))

;; Draw background and title
(def (draw-layout! canvas layout)
  ;; White background
  {canvas.canvas-set-color! [1.0 1.0 1.0]}
  {canvas.canvas-draw-rect! 0 0 (chart-layout-width layout) (chart-layout-height layout) #t}
  ;; Title
  (when (chart-layout-title layout)
    {canvas.canvas-set-color! [0.0 0.0 0.0]}
    {canvas.canvas-set-font! "sans-serif" 16 "bold"}
    {canvas.canvas-draw-text!
      (/ (chart-layout-width layout) 2)
      25
      (truncate-title (chart-layout-title layout))
      "middle" "auto"}))
