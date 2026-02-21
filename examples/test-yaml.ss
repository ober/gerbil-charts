#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Test YAML support

(import :std/text/json
        :gerbil-charts/yaml
        :gerbil-charts/charts/graph)

;; Test 1: Check if YAML is available
(displayln "=== Checking YAML Support ===")
(if (yaml-available?)
  (displayln "✓ YAML support is available (gerbil-libyaml)")
  (begin
    (displayln "✗ YAML support not available")
    (exit 1)))

(displayln)

;; Test 2: Parse a simple YAML string
(displayln "=== Parsing Simple YAML ===")
(def simple-yaml "
type: digraph
name: Test
nodes:
  - id: A
    label: Hello
  - id: B
    label: World
edges:
  - from: A
    to: B
")

(def parsed (yaml-string->json simple-yaml))
(displayln "Parsed YAML:")
(displayln "  Type: " (hash-ref parsed "type"))
(displayln "  Name: " (hash-ref parsed "name"))
(displayln "  Nodes: " (length (hash-ref parsed "nodes")) " nodes")
(displayln)

;; Test 3: Render flowchart example
(displayln "=== Rendering Flowchart Example ===")
(graph-chart-from-file "examples/beginner-flowchart.yaml"
                       output-file: "examples/flowchart-output.svg")
(displayln "✓ Created: examples/flowchart-output.svg")

;; Test 4: Render data pipeline example
(displayln "=== Rendering Data Pipeline Example ===")
(graph-chart-from-file "examples/data-pipeline-simple.yaml"
                       output-file: "examples/pipeline-output.svg")
(displayln "✓ Created: examples/pipeline-output.svg")

;; Test 5: Render org chart example
(displayln "=== Rendering Organization Chart Example ===")
(graph-chart-from-file "examples/organization-chart.yaml"
                       output-file: "examples/org-chart-output.svg")
(displayln "✓ Created: examples/org-chart-output.svg")

;; Test 6: Render website flow example
(displayln "=== Rendering Website Flow Example ===")
(graph-chart-from-file "examples/website-flow.yaml"
                       output-file: "examples/website-flow-output.svg")
(displayln "✓ Created: examples/website-flow-output.svg")

;; Test 7: Render network diagram example
(displayln "=== Rendering Network Diagram Example ===")
(graph-chart-from-file "examples/network-diagram.yaml"
                       output-file: "examples/network-diagram-output.svg")
(displayln "✓ Created: examples/network-diagram-output.svg")

(displayln)
(displayln "=== All Tests Passed! ===")
(displayln)
(displayln "View the generated diagrams:")
(displayln "  open examples/*-output.svg")
