# gerbil-charts

A Gerbil Scheme library for creating charts and diagrams.

## Features

### Time-Series Charts
- **Line charts**: Render time-series data with multiple series
- **Bar charts**: Create bar graphs for categorical data
- **Scatter plots**: Visualize point data distributions
- **Area charts**: Fill areas under curves

### Architecture Diagrams (NEW!)
- **Graphviz integration**: Create flowcharts, architecture diagrams, and data pipelines
- **JSON schema**: Describe graphs in JSON format
- **S-expression DSL**: Build graphs programmatically in Gerbil
- **Multiple engines**: Support for dot, neato, fdp, circo, and more
- **Rich output**: Export to SVG, PNG, PDF, and other formats

## Backends

- **SVG**: Scalable vector graphics (via gerbil-svg)
- **Cairo**: High-quality rasterization (PNG, PDF) via gerbil-cairo
- **Graphviz**: DOT language rendering via dot/neato/fdp engines

## Installation

```bash
# Clone the repository
cd ~/mine/gerbil-charts

# Link dependencies (if not already linked)
gerbil pkg link gerbil-svg ~/mine/gerbil-svg
gerbil pkg link gerbil-cairo ~/mine/gerbil-cairo

# Build dependencies
cd ~/mine/gerbil-svg && gerbil build
cd ~/mine/gerbil-cairo && gerbil build

# Build gerbil-charts
cd ~/mine/gerbil-charts
make build
```

## Quick Start

### Time-Series Chart

```bash
# Create a JSON file with chart data
cat > data.json << EOF
{
  "title": "CPU Usage",
  "width": 800,
  "height": 400,
  "series": [
    {
      "label": "server-1",
      "points": [[1609459200, 45.2], [1609459260, 52.1], [1609459320, 48.7]]
    }
  ]
}
EOF

# Render to SVG
cat data.json | ./bin/gerbil-charts output.svg
```

### Architecture Diagram

```scheme
#!/usr/bin/env gxi
(import :std/text/json
        :gerbil-charts/charts/graph)

(def pipeline-json
  (hash ("graph_type" "digraph")
        ("name" "DataPipeline")
        ("nodes" [
          (hash ("id" "Start") ("label" "Begin") ("shape" "oval") ("fillcolor" "lightgreen"))
          (hash ("id" "Process") ("label" "Process Data") ("shape" "box"))
          (hash ("id" "End") ("label" "Finish") ("shape" "oval") ("fillcolor" "lightblue"))])
        ("edges" [
          (hash ("from" "Start") ("to" "Process") ("label" "input"))
          (hash ("from" "Process") ("to" "End") ("label" "output"))])))

(graph-chart pipeline-json
             engine: 'dot
             format: 'svg
             output-file: "pipeline.svg")
```

Or using the S-expression DSL:

```scheme
#!/usr/bin/env gxi
(import :gerbil-charts/graphviz)

(def pipeline
  (digraph 'DataPipeline
    (attr 'node (cons 'shape "box") (cons 'style "filled,rounded"))
    (node 'Start label: "Begin" fillcolor: "lightgreen" shape: "oval")
    (node 'Process label: "Process Data")
    (node 'End label: "Finish" fillcolor: "lightblue" shape: "oval")
    (edge 'Start 'Process label: "input")
    (edge 'Process 'End label: "output")))

(render-dot-to-file (dot->string pipeline) "pipeline.svg"
                    engine: 'dot format: 'svg)
```

## Documentation

- [Graphviz Integration Guide](GRAPHVIZ_INTEGRATION.md) - Detailed guide for creating architecture diagrams
- [Examples](examples/) - Sample code and JSON files

## Examples

The `examples/` directory contains:
- `pipeline-graph.json`: Multi-stage data pipeline with clusters
- `test-graphviz.ss`: Demonstration of both DSL and JSON approaches
- `pipeline.svg`: Generated output

## Dependencies

- Gerbil Scheme (v0.18+)
- gerbil-svg (for SVG backend)
- gerbil-cairo (for PNG/PDF backends)
- Graphviz (for diagram generation) - install via: `brew install graphviz` or `apt install graphviz`

## Use Cases

### Time-Series Charts
- Monitoring dashboards (CPU, memory, network metrics)
- Financial data visualization
- Scientific data plotting
- IoT sensor data

### Architecture Diagrams
- Data pipeline architectures
- Microservice architectures
- State machines and workflows
- Dependency graphs
- Network topologies

## API Reference

### Time-Series Charts

```scheme
(import :gerbil-charts/charts/line)

(line-chart dataset
            width: 800
            height: 400
            title: "My Chart"
            backend: 'svg
            filename: "output.svg")
```

### Architecture Diagrams

```scheme
(import :gerbil-charts/charts/graph)

;; From JSON
(graph-chart json-hash
             engine: 'dot
             format: 'svg
             output-file: "diagram.svg")

;; From DOT string
(graph-chart dot-string
             engine: 'neato
             format: 'png
             output-file: "diagram.png")
```

### DSL Functions

```scheme
(import :gerbil-charts/graphviz)

;; Graph constructors
(digraph name . body)           ;; Directed graph
(graph name . body)             ;; Undirected graph
(subgraph name . body)          ;; Subgraph (use cluster_* for clusters)

;; Graph elements
(node id . attrs)               ;; Node with keyword attributes
(edge from to . attrs)          ;; Edge with keyword attributes
(attr type . attrs)             ;; Set default attributes (type: graph, node, edge)

;; Rendering
(dot->string graph-expr)        ;; Convert to DOT string
(render-dot dot-string          ;; Render DOT to output format
            engine: 'dot
            format: 'svg)
```

## Building

```bash
make build    # Compile all modules
make clean    # Clean compiled artifacts
make test     # Run tests
make install  # Install to system (optional)
```

## License

Apache License 2.0

## Contributing

Contributions welcome! Please see the examples directory for code patterns.
