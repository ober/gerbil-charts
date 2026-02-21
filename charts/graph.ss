;;; -*- Gerbil -*-
;;; Graphviz graph chart
;;; Converts JSON/YAML graph descriptions to DOT format and renders via graphviz
(export graph-chart graph-chart-from-file)

(import :std/format
        :std/text/json
        :std/misc/string
        :std/misc/ports
        :gerbil-charts/graphviz
        :gerbil-charts/yaml)

;; High-level: create a complete graphviz diagram from JSON, YAML, or DOT
(def (graph-chart json-or-yaml-or-dot
                  engine: (engine 'dot)
                  format: (format 'svg)
                  output-file: (output-file #f))
  (let* ((dot-string
          (cond
            ;; If it's a string, treat as DOT source
            ((string? json-or-yaml-or-dot) json-or-yaml-or-dot)
            ;; Otherwise it's a hash-table (from JSON or YAML)
            (else (dot->string (json->dot json-or-yaml-or-dot)))))
         (result (render-dot dot-string engine: engine format: format)))

    ;; Write to file if specified
    (when output-file
      (call-with-output-file output-file
        (lambda (port) (display result port))))

    result))

;; Create diagram from a file (auto-detects YAML, JSON, or DOT format)
(def (graph-chart-from-file filename
                           engine: (engine 'dot)
                           format: (format 'svg)
                           output-file: (output-file #f))
  (let* ((is-yaml? (or (string-suffix? ".yaml" filename)
                       (string-suffix? ".yml" filename)))
         (is-json? (string-suffix? ".json" filename))
         (is-dot? (string-suffix? ".dot" filename))
         (data
          (cond
            (is-yaml? (yaml-file->json filename))
            (is-json? (call-with-input-file filename read-json))
            (is-dot? (call-with-input-file filename
                      (lambda (port) (read-all-as-string port))))
            (else
             (error "Unknown file type. Use .yaml, .json, or .dot extension"
                   filename)))))
    (graph-chart data engine: engine format: format output-file: output-file)))
