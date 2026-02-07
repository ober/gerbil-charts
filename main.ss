;;; -*- Gerbil -*-
;;; CLI entry point for gerbil-charts
;;; Two modes:
;;;   Single chart: gerbil-charts output.png < chart_data.json
;;;   Dashboard:    gerbil-charts --dashboard --url URL --instance INST --output DIR
(export main)

(import :std/cli/getopt
        :std/sugar
        :std/text/json
        :std/format
        :gerbil-charts/data
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/charts/line
        :gerbil-charts/prometheus
        :gerbil-charts/dashboard)

;; Default color palette (matches prometheus.ss)
(def default-colors
  [[0.2 0.4 0.8]    ;; blue
   [0.8 0.2 0.2]    ;; red
   [0.2 0.7 0.2]    ;; green
   [0.8 0.6 0.0]    ;; orange
   [0.6 0.2 0.8]    ;; purple
   [0.0 0.7 0.7]    ;; cyan
   [0.8 0.4 0.6]    ;; pink
   [0.4 0.4 0.4]])  ;; gray

;; Parse JSON input into a dataset.
;; Expected format:
;;   {"mode":"mini","title":"CPU","width":800,"height":400,
;;    "series":[{"label":"cpu","points":[[1609459200,0.45],[1609459260,0.52]]}]}
(def (json->dataset json)
  (let* ((series-json (hash-ref json "series"))
         (title (or (hash-get json "title") ""))
         (series-list
           (map (lambda (s i)
                  (let* ((label (or (hash-get s "label") "series"))
                         (pts (hash-ref s "points"))
                         (points (map (lambda (pt)
                                        (make-data-point (car pt) (cadr pt) #f))
                                      pts))
                         (color (list-ref default-colors
                                         (modulo i (length default-colors)))))
                    (make-data-series label points color 'solid)))
                series-json
                (iota (length series-json)))))
    (make-dataset series-list title)))

;; Compute data time range in seconds from a dataset
(def (dataset-range-seconds ds)
  (let-values (((x-min x-max) (dataset-x-range ds)))
    (- x-max x-min)))

;; Run single chart mode: read JSON from stdin, render chart to output file
(def (run-single-chart opt)
  (let-hash opt
    (let* ((output .output-file)
           (width (or .?width 800))
           (height (or .?height 400))
           (bk (string->symbol (or .?backend "svg")))
           (start-time (current-second))
           ;; Read JSON from stdin
           (json (read-json (current-input-port)))
           ;; Override dimensions from JSON if present
           (width (or (hash-get json "width") width))
           (height (or (hash-get json "height") height))
           (mode (or (hash-get json "mode") "normal"))
           (json-title (hash-get json "title"))
           (title (or .?title json-title))
           ;; Parse into dataset
           (ds (json->dataset json))
           ;; Downsample
           (max-pts (if (equal? mode "mini") 100 500))
           (ds (make-dataset
                 (map (lambda (s) (downsample s max-pts))
                      (dataset-series ds))
                 (dataset-title ds)))
           ;; Compute time range for axis formatting
           (range-secs (dataset-range-seconds ds))
           ;; Render
           (result (line-chart ds
                     width: width height: height
                     title: (and title (truncate-title title))
                     backend: bk
                     filename: output
                     x-label: #f y-label: #f))
           (elapsed (- (current-second) start-time)))

      ;; For SVG backend, write the returned string to output file
      (when (and (eq? bk 'svg) (string? result))
        (call-with-output-file output
          (lambda (port) (display result port))))

      (fprintf (current-error-port) "Rendered ~a (~ax~a, ~a) in ~a ms\n"
               output width height bk
               (inexact->exact (floor (* elapsed 1000)))))))

;; Run dashboard mode: query Prometheus and generate charts
(def (run-dashboard opt)
  (let-hash opt
    (let ((url (or .?url (error "--url is required for dashboard mode")))
          (instance (or .?instance (error "--instance is required for dashboard mode")))
          (output-dir (or .?output "charts"))
          (range (or .?range "24h"))
          (prefix (or .?prefix #f))
          (max-charts (or .?max 0))
          (concurrency (or .?concurrency 20))
          (bk (string->symbol (or .?backend "png")))
          (width (or .?width 800))
          (height (or .?height 400))
          (step (or .?step "60s")))
      (generate-dashboard url instance output-dir
        range: range
        prefix: prefix
        max-charts: max-charts
        concurrency: concurrency
        backend: bk
        width: width
        height: height
        step: step))))

(def (main . args)
  (call-with-getopt
    (lambda (opt)
      (let-hash opt
        (if .?dashboard
          (run-dashboard opt)
          (begin
            (unless .?output-file
              (error "Output filename required (positional argument)"))
            (run-single-chart opt)))))
    args
    program: "gerbil-charts"
    help: "Render line charts from JSON data or Prometheus metrics"
    ;; Mode flag
    (flag 'dashboard "--dashboard"
      help: "Dashboard mode: query Prometheus and generate charts for all metrics")
    ;; Single chart options
    (optional-argument 'output-file
      help: "Output filename (e.g. output.svg, output.png)")
    (option 'title "--title" "-t"
      help: "Chart title (overrides JSON title)"
      default: #f)
    ;; Shared options
    (option 'width "--width" "-W"
      help: "Chart width in pixels"
      value: string->number
      default: #f)
    (option 'height "--height" "-H"
      help: "Chart height in pixels"
      value: string->number
      default: #f)
    (option 'backend "--backend" "-b"
      help: "Rendering backend: svg, png, pdf"
      default: #f)
    ;; Dashboard options
    (option 'url "--url" "-u"
      help: "Prometheus base URL (dashboard mode)"
      default: #f)
    (option 'instance "--instance" "-i"
      help: "Prometheus instance label (dashboard mode)"
      default: #f)
    (option 'output "--output" "-o"
      help: "Output directory (dashboard mode, default: charts)"
      default: #f)
    (option 'range "--range" "-r"
      help: "Time range (e.g. 1h, 24h, 7d; dashboard mode, default: 24h)"
      default: #f)
    (option 'prefix "--prefix"
      help: "Metric name prefix filter (dashboard mode)"
      default: #f)
    (option 'max "--max" "-m"
      help: "Maximum number of charts (dashboard mode, 0=unlimited)"
      value: string->number
      default: #f)
    (option 'concurrency "--concurrency" "-c"
      help: "Concurrent chart generation limit (dashboard mode, default: 20)"
      value: string->number
      default: #f)
    (option 'step "--step"
      help: "Query step interval (dashboard mode, default: 60s)"
      default: #f)))
