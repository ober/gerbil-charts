# Graphviz Integration for gerbil-charts

## Overview

The gerbil-charts library supports creating architecture diagrams, flowcharts, and data pipeline visualizations using Graphviz via native FFI bindings (no external `dot` binary required). This integration allows you to:

1. Generate DOT format graphs from JSON descriptions
2. Use a Scheme DSL for programmatic graph construction
3. Render graphs to SVG, PNG, PDF, or other formats supported by Graphviz

## Architecture

### New Modules

- **`graphviz.ss`**: Core DOT generation and rendering
  - S-expression DSL for building graphs: `digraph`, `graph`, `node`, `edge`, `subgraph`, `attr`
  - JSON → DOT conversion: `json->dot`
  - DOT rendering via graphviz FFI: `render-dot`, `render-dot-to-file`
  - DOT string generation: `dot->string`

- **`charts/graph.ss`**: High-level graph chart API
  - `graph-chart`: Unified interface for creating graphs from JSON or DOT strings
  - Supports all graphviz engines: dot, neato, fdp, circo, twopi, etc.
  - Output formats: SVG, PNG, PDF, and more

## Usage

### Method 1: JSON Graph Descriptions

Create a JSON file describing your graph:

```json
{
  "graph_type": "digraph",
  "name": "MyPipeline",
  "nodes": [
    {"id": "Start", "label": "Begin", "shape": "oval", "fillcolor": "lightgreen"},
    {"id": "Process", "label": "Process Data", "shape": "box"},
    {"id": "End", "label": "Finish", "shape": "oval", "fillcolor": "lightblue"}
  ],
  "edges": [
    {"from": "Start", "to": "Process", "label": "input"},
    {"from": "Process", "to": "End", "label": "output"}
  ]
}
```

Render it:

```scheme
(import :std/text/json
        :gerbil-charts/charts/graph)

(def json (call-with-input-file "pipeline.json" read-json))
(graph-chart json
             engine: 'dot
             format: 'svg
             output-file: "pipeline.svg")
```

### Method 2: S-expression DSL

Build graphs programmatically:

```scheme
(import :gerbil-charts/graphviz)

(def my-graph
  (digraph 'Pipeline
    ;; Global node attributes
    (attr 'node (cons 'shape "box") (cons 'style "filled,rounded"))

    ;; Nodes
    (node 'Start label: "Begin Process" fillcolor: "lightgreen" shape: "oval")
    (node 'Step1 label: "Download Files")
    (node 'Step2 label: "Transform Data")
    (node 'End label: "Complete" fillcolor: "lightblue" shape: "oval")

    ;; Edges
    (edge 'Start 'Step1 label: "fetch")
    (edge 'Step1 'Step2 label: "process")
    (edge 'Step2 'End)))

;; Generate DOT string
(displayln (dot->string my-graph))

;; Or render to file
(render-dot-to-file (dot->string my-graph) "output.svg"
                    engine: 'dot
                    format: 'svg)
```

### Method 3: Clustered Subgraphs

For complex diagrams with grouped components:

```scheme
(def pipeline
  (digraph 'ComplexPipeline
    (attr 'graph (cons 'rankdir "TB"))

    ;; Discovery stage cluster
    (subgraph 'cluster_Discovery
      (attr 'graph (cons 'label "Discovery Stage")
                   (cons 'style "filled")
                   (cons 'color "lightgrey"))
      (node 'List label: "List Files")
      (node 'Filter label: "Filter New"))

    ;; Processing stage cluster
    (subgraph 'cluster_Processing
      (attr 'graph (cons 'label "Processing")
                   (cons 'style "filled")
                   (cons 'color "lightblue"))
      (node 'Download label: "Download")
      (node 'Transform label: "Transform")
      (node 'Load label: "Load to DB"))

    ;; Cross-cluster edges
    (edge 'Filter 'Download)))
```

## JSON Schema Reference

### Graph Object

- `graph_type`: `"digraph"` (directed) or `"graph"` (undirected)
- `name`: Graph name (symbol)
- `graph_attrs`: Hash of graph-level attributes (optional)
- `node_attrs`: Default node attributes (optional)
- `edge_attrs`: Default edge attributes (optional)
- `nodes`: Array of node objects
- `edges`: Array of edge objects
- `clusters`: Array of subgraph objects (optional)

### Node Object

- `id`: Unique node identifier (required)
- `label`: Display label (optional, defaults to id)
- `shape`: Node shape (`"box"`, `"oval"`, `"diamond"`, `"cylinder"`, etc.)
- `fillcolor`: Fill color (HTML color names or hex)
- `style`: Style attributes (`"filled"`, `"rounded"`, `"filled,rounded"`)
- Any other graphviz node attributes

### Edge Object

- `from`: Source node id (required)
- `to`: Target node id (required)
- `label`: Edge label (optional)
- `style`: Edge style (`"solid"`, `"dashed"`, `"dotted"`)
- Any other graphviz edge attributes

### Cluster Object

- `name`: Cluster subgraph name (must start with `"cluster_"`)
- `label`: Cluster label
- `attrs`: Hash of subgraph attributes
- `nodes`: Array of node IDs belonging to this cluster

## Available Graphviz Engines

- `dot`: Hierarchical/layered graphs (default)
- `neato`: Spring model layouts
- `fdp`: Force-directed placement
- `sfdp`: Scalable force-directed placement (large graphs)
- `circo`: Circular layout
- `twopi`: Radial layout
- `osage`: Array-based layouts
- `patchwork`: Squarified tree maps

## Output Formats

All formats supported by graphviz:
- `svg`: Scalable Vector Graphics (recommended)
- `png`: Raster image
- `pdf`: Portable Document Format
- `ps`: PostScript
- `dot`: DOT source (useful for debugging)

## Example: Converting Your Architecture Diagrams

1. Convert the DOT file to JSON using a conversion script
2. Use the JSON with `graph-chart` to render
3. Or keep the DOT file and pass it directly:

```scheme
(import :gerbil-charts/charts/graph)

(def dot-string (call-with-input-file "pipeline.dot"
                  (lambda (port) (read-line port #f))))

(graph-chart dot-string
             engine: 'dot
             format: 'svg
             output-file: "pipeline.svg")
```

## Integration with Existing CLI

The main CLI (`main.ss`) can be extended to support graphviz charts by adding a `--type` option:

```bash
# Current usage (time-series charts)
echo '{"series":[...]}' | gerbil-charts output.svg

# Future usage (graphviz diagrams)
cat pipeline.json | gerbil-charts --type graph output.svg
```

## Dependencies

- Gerbil packages: `gerbil-svg`, `gerbil-cairo` (for existing chart types), `gerbil-graphviz` (FFI bindings)
- Graphviz C libraries (libgvc, libcgraph) — install via: `brew install graphviz` or `apt install libgraphviz-dev`
- No external `dot`/`neato` binaries needed — rendering is done via native FFI calls to the Graphviz C library
- The static binary (`make static`) bundles Graphviz entirely, requiring zero external dependencies

## Next Steps

1. **Natural Language Processing**: Parse text descriptions to generate graphs
2. **Template Library**: Pre-built templates for common diagram types (FSM, architecture, data flow)
3. **CLI Integration**: Update `main.ss` to support `--type graph` option
4. **Interactive Editing**: Allow incremental graph updates
5. **Export Formats**: Support exporting to Mermaid, PlantUML, etc.
