;;; -*- Gerbil -*-
(import :std/test
        :gerbil-charts/data
        :gerbil-charts/charts/line
        :gerbil-charts/canvas/svg)
(export charts-test)

(def charts-test
  (test-suite "chart integration"
    (test-case "line chart produces SVG string"
      (let* ((points (map (lambda (x) (make-data-point x (* x x) #f))
                          '(0 1 2 3 4 5 6 7 8 9 10)))
             (series (make-data-series "y=x^2" points [0.2 0.4 0.8] 'solid))
             (ds (make-dataset [series] "Quadratic"))
             (svg-str (line-chart ds title: "Test Chart")))
        (check (string? svg-str) ? values)
        (check (string-contains svg-str "<svg") ? values)
        (check (string-contains svg-str "Test Chart") ? values)))))
