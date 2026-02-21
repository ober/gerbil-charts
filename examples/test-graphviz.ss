#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Test graphviz integration

(import :std/text/json
        :gerbil-charts/graphviz
        :gerbil-charts/charts/graph)

;; Example 1: Generate DOT from S-expression DSL
(def simple-graph
  (digraph 'MyGraph
    (attr 'node (cons 'shape "box") (cons 'style "filled"))
    (node 'A label: "Start")
    (node 'B label: "Process")
    (node 'C label: "End")
    (edge 'A 'B label: "step 1")
    (edge 'B 'C label: "step 2")))

(displayln "=== Simple Graph DOT ===")
(displayln (dot->string simple-graph))
(displayln)

;; Example 2: Load and convert JSON graph description
(def (test-json-graph)
  (let* ((json-file "examples/pipeline-graph.json")
         (json (call-with-input-file json-file read-json))
         (dot-expr (json->dot json)))
    (displayln "=== Pipeline Graph DOT ===")
    (displayln (dot->string dot-expr))
    (displayln)

    ;; Render to SVG
    (displayln "=== Rendering to examples/pipeline.svg ===")
    (graph-chart json
                 engine: 'dot
                 format: 'svg
                 output-file: "examples/pipeline.svg")
    (displayln "Done!")))

;; Run tests
(test-json-graph)
