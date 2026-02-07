;;; -*- Gerbil -*-
;;; Dashboard mode: bulk concurrent chart generation for all metrics of an instance
(export generate-dashboard)

(import :std/misc/wg
        :std/misc/atom
        :std/text/json
        :gerbil-charts/data
        :gerbil-charts/prometheus
        :gerbil-charts/axis
        :gerbil-charts/layout
        :gerbil-charts/charts/line)

;; Generate charts for all metrics of a Prometheus instance.
;; Returns a list of (metric-name . filename) pairs for generated charts.
;; Also writes a JSON manifest to stdout.
(def (generate-dashboard
      prometheus-url instance output-dir
      range: (range "24h")
      prefix: (prefix #f)
      max-charts: (max-charts 0)
      concurrency: (concurrency 20)
      backend: (backend 'png)
      width: (width 800)
      height: (height 400)
      step: (step "60s")
      x-label: (x-label #f)
      y-label: (y-label #f))

  ;; Ensure output directory exists
  (create-directory* output-dir)

  ;; Parse range to seconds for time formatting and query window
  (let* ((range-secs (parse-duration range))
         (end-ts (inexact->exact (floor (exact->inexact
                   (time->seconds (##current-time))))))
         (start-ts (- end-ts range-secs))
         (start-str (number->string start-ts))
         (end-str (number->string end-ts))

         ;; Discover metrics
         (metrics (prometheus-get-metrics prometheus-url instance prefix: prefix))
         (metrics (if (and (> max-charts 0) (> (length metrics) max-charts))
                    (let loop ((rest metrics) (n 0) (acc []))
                      (if (or (null? rest) (>= n max-charts))
                        (reverse acc)
                        (loop (cdr rest) (+ n 1) (cons (car rest) acc))))
                    metrics))

         ;; Results accumulator
         (results (atom []))
         (errors (atom []))

         ;; File extension based on backend
         (ext (case backend
                ((png) ".png")
                ((svg) ".svg")
                ((pdf) ".pdf")
                ((cairo-svg) ".svg")
                (else ".png"))))

    ;; Fetch and render charts concurrently
    (let ((wg (make-wg concurrency)))
      (for-each
        (lambda (metric)
          (wg-add! wg
            (lambda ()
              (with-catch
                (lambda (e)
                  (atom-swap! errors
                    (lambda (lst)
                      (cons (cons metric
                                  (if (error-object? e)
                                    (error-message e)
                                    (with-output-to-string (lambda () (display e)))))
                            lst))))
                (lambda ()
                  (let* ((query (string-append metric "{instance=\"" instance "\"}"))
                         (json (prometheus-query-range
                                 prometheus-url query start-str end-str step: step))
                         (title (truncate-title metric))
                         (ds (prometheus-result->dataset json title: title))
                         (series-list (dataset-series ds)))
                    ;; Skip empty results
                    (when (pair? series-list)
                      ;; Downsample each series
                      (let* ((max-pts (inexact->exact (floor (/ width 2.0))))
                             (ds (make-dataset
                                   (map (lambda (s) (downsample s max-pts)) series-list)
                                   (dataset-title ds)))
                             ;; Generate filename from metric name
                             (safe-name (let loop ((i 0) (acc []))
                                          (if (>= i (string-length metric))
                                            (list->string (reverse acc))
                                            (let ((c (string-ref metric i)))
                                              (loop (+ i 1)
                                                    (cons (if (or (char-alphabetic? c)
                                                                  (char-numeric? c)
                                                                  (char=? c #\_)
                                                                  (char=? c #\-))
                                                            c #\_)
                                                          acc))))))
                             (filename (string-append output-dir "/" safe-name ext)))

                        ;; Render chart
                        (let ((result (line-chart ds
                                        width: width height: height
                                        title: title
                                        backend: backend
                                        filename: filename
                                        x-label: x-label
                                        y-label: y-label)))
                          ;; For SVG backend, write the string to file
                          (when (and (eq? backend 'svg) (string? result))
                            (call-with-output-file filename
                              (lambda (port) (display result port)))))

                        (atom-swap! results
                          (lambda (lst)
                            (cons (cons metric filename) lst)))))))))))
        metrics)
      (wg-wait! wg))

    ;; Collect results
    (let ((chart-list (atom-deref results))
          (error-list (atom-deref errors)))

      ;; Write JSON manifest to stdout
      (let ((manifest
              (hash ("instance" instance)
                    ("range" range)
                    ("charts_generated" (length chart-list))
                    ("errors" (length error-list))
                    ("charts" (map (lambda (pair)
                                    (hash ("metric" (car pair))
                                          ("file" (cdr pair))))
                                  chart-list))
                    ("error_details" (map (lambda (pair)
                                           (hash ("metric" (car pair))
                                                 ("error" (cdr pair))))
                                         error-list)))))
        (write-json manifest)
        (newline))

      chart-list)))
