;;; -*- Gerbil -*-
;;; Prometheus HTTP API client
(export
  prometheus-query-range
  prometheus-result->dataset
  parse-duration
  prometheus-query
  prometheus-get-metrics)

(import :std/net/request
        :std/text/json
        :std/sort
        :gerbil-charts/data)

;; Query Prometheus for time-series data
;; Returns raw JSON response as hash table
(def (prometheus-query-range base-url query start end step: (step "60s"))
  (let* ((url (string-append base-url "/api/v1/query_range"))
         (resp (http-get url
                 params: [["query" . query]
                          ["start" . start]
                          ["end" . end]
                          ["step" . step]])))
    (unless (= (request-status resp) 200)
      (error "Prometheus query failed" (request-status resp) (request-text resp)))
    (request-json resp)))

;; Default color palette for multiple series
(def default-colors
  [[0.2 0.4 0.8]    ;; blue
   [0.8 0.2 0.2]    ;; red
   [0.2 0.7 0.2]    ;; green
   [0.8 0.6 0.0]    ;; orange
   [0.6 0.2 0.8]    ;; purple
   [0.0 0.7 0.7]    ;; cyan
   [0.8 0.4 0.6]    ;; pink
   [0.4 0.4 0.4]])  ;; gray

;; Convert Prometheus JSON response to a dataset
(def (prometheus-result->dataset json-response title: (title ""))
  (let* ((data (hash-ref json-response "data"))
         (results (hash-ref data "result")))
    (make-dataset
      (map (lambda (result i)
             (let* ((metric (hash-ref result "metric"))
                    (name (or (hash-get metric "__name__")
                              (hash-get metric "instance")
                              "unknown"))
                    (vals (hash-ref result "values"))
                    (points (map (lambda (v)
                                  (make-data-point
                                    (car v)
                                    (string->number (cadr v))
                                    #f))
                                vals))
                    (color (list-ref default-colors
                                    (modulo i (length default-colors)))))
               (make-data-series name points color 'solid)))
           results
           (iota (length results)))
      title)))

;; Parse a duration string like "30s", "5m", "2h", "7d", "1w" into seconds
(def (parse-duration str)
  (let* ((len (string-length str))
         (suffix (string-ref str (- len 1)))
         (num-str (substring str 0 (- len 1)))
         (num (string->number num-str)))
    (unless num
      (error "Invalid duration number" num-str))
    (case suffix
      ((#\s) num)
      ((#\m) (* num 60))
      ((#\h) (* num 3600))
      ((#\d) (* num 86400))
      ((#\w) (* num 604800))
      (else (error "Unknown duration suffix" suffix)))))

;; Prometheus instant query (/api/v1/query)
;; Returns raw JSON response as hash table
(def (prometheus-query base-url query time: (time #f))
  (let* ((url (string-append base-url "/api/v1/query"))
         (params (if time
                   [["query" . query] ["time" . time]]
                   [["query" . query]]))
         (resp (http-get url params: params)))
    (unless (= (request-status resp) 200)
      (error "Prometheus query failed" (request-status resp) (request-text resp)))
    (request-json resp)))

;; Query all metric names for a given instance, optionally filtered by prefix
(def (prometheus-get-metrics base-url instance prefix: (prefix #f))
  (let* ((query (if prefix
                  (string-append "{__name__=~\"" prefix ".*\",instance=\"" instance "\"}")
                  (string-append "{instance=\"" instance "\"}")))
         (json (prometheus-query base-url query))
         (data (hash-ref json "data"))
         (results (hash-ref data "result"))
         (names (map (lambda (r)
                       (hash-ref (hash-ref r "metric") "__name__"))
                     results)))
    ;; Deduplicate and sort
    (let ((seen (make-hash-table)))
      (sort (let loop ((rest names) (acc []))
              (cond
                ((null? rest) (reverse acc))
                ((hash-get seen (car rest))
                 (loop (cdr rest) acc))
                (else
                 (hash-put! seen (car rest) #t)
                 (loop (cdr rest) (cons (car rest) acc)))))
            string<?))))
