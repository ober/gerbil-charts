;;; -*- Gerbil -*-
;;; Canvas interface: backend-agnostic drawing abstraction
;;;
;;; Methods (called via {obj.method args} dispatch):
;;;   canvas-width, canvas-height
;;;   canvas-draw-line!, canvas-draw-rect!, canvas-draw-circle!
;;;   canvas-draw-text!, canvas-draw-path!
;;;   canvas-set-color!, canvas-set-line-width!, canvas-set-font!
;;;   canvas-save!, canvas-restore!, canvas-clip-rect!
;;;   canvas-finish!, canvas-to-string
(export Canvas is-Canvas?)

(interface Canvas
  (canvas-width)
  (canvas-height)
  (canvas-draw-line! x1 y1 x2 y2)
  (canvas-draw-rect! x y w h fill?)
  (canvas-draw-circle! cx cy r fill?)
  (canvas-draw-text! x y text anchor baseline)
  (canvas-draw-path! points close?)
  (canvas-set-color! color)
  (canvas-set-line-width! w)
  (canvas-set-font! family size weight)
  (canvas-save!)
  (canvas-restore!)
  (canvas-clip-rect! x y w h)
  (canvas-finish!)
  (canvas-to-string))
