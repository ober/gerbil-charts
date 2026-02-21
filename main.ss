;;; -*- Gerbil -*-
;;; CLI entry point for gerbil-charts
;;; Render line charts from JSON data (reads from stdin) or diagrams from YAML
(export main)

(import :std/cli/getopt
        :std/sugar
        :std/text/json
        :std/format
        :gerbil-charts/data
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/charts/line
        :gerbil-charts/yaml
        :gerbil-charts/graphviz)

;; Default color palette
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

;; Detect if JSON is a graphviz diagram or chart data
(def (json->graphviz-diagram? json)
  (or (hash-get json "type")       ;; YAML format has "type: digraph"
      (hash-get json "graph_type") ;; JSON format has "graph_type"
      (hash-get json "nodes")))    ;; Has nodes list

;; Run graphviz diagram mode: render diagram from YAML/JSON
(def (run-diagram-mode json output backend)
  (let* ((start-time (current-second))
         ;; Convert YAML format to JSON format if needed
         (json-converted
          (if (hash-get json "type")
            ;; YAML format: convert to graphviz JSON format
            (let* ((graph-attrs (or (hash-get json "graph") (hash)))
                   (defaults (or (hash-get json "defaults") (hash)))
                   (node-defaults (or (hash-get defaults "node") (hash)))
                   (nodes (or (hash-get json "nodes") []))
                   (edges (or (hash-get json "edges") [])))
              (hash
               ("graph_type" (or (hash-get json "type") "digraph"))
               ("name" (or (hash-get json "name") "G"))
               ("graph_attrs" graph-attrs)
               ("node_attrs" node-defaults)
               ("nodes" nodes)
               ("edges" edges)))
            ;; Already in graphviz JSON format
            json))
         ;; Convert to DOT
         (dot-expr (json->dot json-converted))
         (dot-string (dot->string dot-expr))
         ;; Determine output format from extension
         (ext (path-extension output))
         (format (cond
                   ((equal? ext ".svg") 'svg)
                   ((equal? ext ".png") 'png)
                   ((equal? ext ".pdf") 'pdf)
                   (else (string->symbol (or backend "svg")))))
         )
    ;; Render directly to file via graphviz FFI
    (render-dot-to-file dot-string output engine: 'dot format: format)

    (let ((elapsed (- (current-second) start-time)))
      (fprintf (current-error-port) "Rendered diagram ~a (~a) in ~a ms\n"
               output format
               (inexact->exact (floor (* elapsed 1000)))))))

;; Run single chart mode: read JSON from stdin, render chart to output file
(def (run-single-chart opt)
  (let-hash opt
    (let* ((output .output-file)
           (width (or .?width 800))
           (height (or .?height 400))
           (bk (string->symbol (or .?backend "svg")))
           (start-time (current-second))
           ;; Read JSON from stdin or YAML file
           (json (if .?yaml
                   (yaml-file->json .yaml)
                   (read-json (current-input-port))))

           ;; Check if this is a diagram or chart
           (is-diagram (json->graphviz-diagram? json)))

      ;; Route to appropriate renderer
      (if is-diagram
        (run-diagram-mode json output .?backend)

        ;; Original chart rendering logic
        (let* (;; Override dimensions from JSON if present
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
                   (inexact->exact (floor (* elapsed 1000)))))))))

;; Helper to get file extension
(def (path-extension path)
  (let ((idx (string-rindex path #\.)))
    (if idx (substring path idx (string-length path)) "")))

(def (main . args)
  (call-with-getopt
    (lambda (opt)
      (let-hash opt
        (unless .?output-file
          (error "Output filename required (positional argument)"))
        (run-single-chart opt)))
    args
    program: "gerbil-charts"
    help: "Render line charts from JSON data (stdin) or diagrams from YAML files"
    (argument 'output-file
      help: "Output filename (e.g. output.svg, output.png)")
    (option 'yaml "--yaml" "-y"
      help: "YAML input file (for diagrams or chart data)"
      default: #f)
    (option 'title "--title" "-t"
      help: "Chart title (overrides JSON title)"
      default: #f)
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
      default: #f)))
