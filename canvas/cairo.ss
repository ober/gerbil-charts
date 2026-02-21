;;; -*- Gerbil -*-
;;; Cairo canvas backend — implements Canvas interface using gerbil-cairo
(export make-cairo-png-canvas make-cairo-svg-canvas make-cairo-pdf-canvas)

(import :gerbil-cairo/cairo
        :gerbil-cairo/libcairo
        :gerbil-charts/canvas)

(defclass cairo-canvas (width height surface cr output-file output-format)
  constructor: :init!)

(defmethod {:init! cairo-canvas}
  (lambda (self w h format filename)
    (set! self.width w)
    (set! self.height h)
    (set! self.output-format format)
    (set! self.output-file filename)
    (set! self.surface
      (case format
        ((png)  (make-image-surface w h))
        ((svg)  (make-svg-surface filename (exact->inexact w) (exact->inexact h)))
        ((pdf)  (make-pdf-surface filename (exact->inexact w) (exact->inexact h)))
        (else (error "Unknown cairo format" format))))
    (set! self.cr (make-cairo-context self.surface))))

;; Factories
(def (make-cairo-png-canvas width height (filename #f))
  (make-cairo-canvas width height 'png filename))

(def (make-cairo-svg-canvas width height filename)
  (make-cairo-canvas width height 'svg filename))

(def (make-cairo-pdf-canvas width height filename)
  (make-cairo-canvas width height 'pdf filename))

;; Canvas interface implementation
(defmethod {canvas-width cairo-canvas}
  (lambda (self) self.width))

(defmethod {canvas-height cairo-canvas}
  (lambda (self) self.height))

(defmethod {canvas-draw-line! cairo-canvas}
  (lambda (self x1 y1 x2 y2)
    (cairo-draw-line! self.cr x1 y1 x2 y2)))

(defmethod {canvas-draw-rect! cairo-canvas}
  (lambda (self x y w h fill?)
    (cairo-draw-rect! self.cr x y w h fill?: fill?)))

(defmethod {canvas-draw-circle! cairo-canvas}
  (lambda (self cx cy r fill?)
    (cairo-draw-circle! self.cr cx cy r fill?: fill?)))

(defmethod {canvas-draw-text! cairo-canvas}
  (lambda (self x y text anchor baseline)
    (let ((x (exact->inexact x))
          (y (exact->inexact y)))
      (cond
        ((equal? anchor "middle")
         (let-values (((w h xb yb xa) (cairo-measure-text self.cr text)))
           (cairo-draw-text! self.cr (- x (/ xa 2.0)) y text)))
        ((equal? anchor "end")
         (let-values (((w h xb yb xa) (cairo-measure-text self.cr text)))
           (cairo-draw-text! self.cr (- x xa) y text)))
        (else
         (cairo-draw-text! self.cr x y text))))))

(defmethod {canvas-draw-path! cairo-canvas}
  (lambda (self points close?)
    (when (pair? points)
      (let ((cmds (let loop ((pts points) (first? #t) (acc []))
                    (if (null? pts)
                      (reverse (if close? (cons ['close] acc) acc))
                      (let ((pt (car pts)))
                        (if first?
                          (loop (cdr pts) #f (cons ['move-to (car pt) (cadr pt)] acc))
                          (loop (cdr pts) #f (cons ['line-to (car pt) (cadr pt)] acc))))))))
        (cairo-draw-path! self.cr cmds fill?: close?)))))

(defmethod {canvas-set-color! cairo-canvas}
  (lambda (self color)
    (cairo-set-color! self.cr color)))

(defmethod {canvas-set-line-width! cairo-canvas}
  (lambda (self w)
    (cairo-set-line-style! self.cr w)))

(defmethod {canvas-set-font! cairo-canvas}
  (lambda (self family size weight)
    (cairo-set-font! self.cr family (exact->inexact size)
                     weight: (if (equal? weight "bold")
                               CAIRO_FONT_WEIGHT_BOLD
                               CAIRO_FONT_WEIGHT_NORMAL))))

(defmethod {canvas-save! cairo-canvas}
  (lambda (self) (cairo_save self.cr)))

(defmethod {canvas-restore! cairo-canvas}
  (lambda (self) (cairo_restore self.cr)))

(defmethod {canvas-clip-rect! cairo-canvas}
  (lambda (self x y w h)
    (void)))  ;; cairo_clip not in FFI bindings; no-op for now

(defmethod {canvas-finish! cairo-canvas}
  (lambda (self)
    (cairo_surface_flush self.surface)
    (when (and (eq? self.output-format 'png) self.output-file)
      (surface-write-png! self.surface self.output-file))
    (surface-finish! self.surface)))

(defmethod {canvas-to-string cairo-canvas}
  (lambda (self) ""))
