;;; -*- Gerbil -*-
(import :std/test
        :std/text/json
        :gerbil-charts/prometheus
        :gerbil-charts/data)
(export prometheus-test)

(def prometheus-test
  (test-suite "prometheus client"
    (test-case "parse prometheus response"
      (let ((mock-json
              (hash ("status" "success")
                    ("data" (hash ("resultType" "matrix")
                                  ("result" [(hash ("metric" (hash ("__name__" "cpu_usage")))
                                                   ("values" [[1609459200 "0.45"]
                                                              [1609459260 "0.52"]
                                                              [1609459320 "0.48"]]))]))))))
        (let ((ds (prometheus-result->dataset mock-json title: "CPU")))
          (check (dataset? ds) ? values)
          (check (= (length (dataset-series ds)) 1) ? values)
          (let ((s (car (dataset-series ds))))
            (check (= (length (data-series-points s)) 3) ? values)
            (check (data-point-y (car (data-series-points s))) => 0.45)))))

    (test-case "parse-duration seconds"
      (check (parse-duration "30s") => 30))

    (test-case "parse-duration minutes"
      (check (parse-duration "5m") => 300))

    (test-case "parse-duration hours"
      (check (parse-duration "2h") => 7200))

    (test-case "parse-duration days"
      (check (parse-duration "7d") => 604800))

    (test-case "parse-duration weeks"
      (check (parse-duration "1w") => 604800))

    (test-case "parse-duration 24h"
      (check (parse-duration "24h") => 86400))))
