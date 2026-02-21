# YAML Support - Complete Integration

**Create beautiful diagrams without programming - just edit simple text files!**

## What's New

gerbil-charts now supports YAML input - a human-friendly format that's easier to read and write than JSON. Perfect for non-technical users who want to create professional diagrams.

## Quick Start (60 Seconds)

```bash
# 1. Copy a template
cp examples/beginner-flowchart.yaml my-diagram.yaml

# 2. Edit in your favorite editor
nano my-diagram.yaml

# 3. Generate your diagram
gerbil-charts --yaml my-diagram.yaml output.svg

# 4. View it!
open output.svg
```

## What's Included

### 📚 Documentation (3 Guides)
1. **QUICK_START.md** - Get started in 3 steps
2. **YAML_GUIDE.md** - Complete beginner-friendly guide (10,000+ words)
3. **YAML_CHEATSHEET.md** - One-page quick reference

### 🎨 Ready-to-Use Templates (5 Examples)
All templates include:
- ✅ Extensive comments explaining every section
- ✅ Easy-to-customize labels and colors
- ✅ Real-world examples
- ✅ Best practices built-in

**Templates:**
1. **beginner-flowchart.yaml** - Approval/decision processes
   - Perfect for: Workflows, approval chains, process documentation
   - Features: Decision diamonds, color-coded paths, clear start/end

2. **data-pipeline-simple.yaml** - ETL/data flow diagrams
   - Perfect for: Data pipelines, system integration, data architecture
   - Features: 3-stage pipeline with clusters, database shapes, clear data flow

3. **organization-chart.yaml** - Team hierarchies
   - Perfect for: Org charts, reporting structures, team layouts
   - Features: Color-coded levels, clean hierarchy, professional styling

4. **website-flow.yaml** - User journeys
   - Perfect for: Website navigation, app flows, user paths
   - Features: Entry/exit points, decision paths, shopping cart flow

5. **network-diagram.yaml** - Infrastructure diagrams
   - Perfect for: System architecture, cloud infrastructure, network topology
   - Features: Network zones, database/storage shapes, clean layout

### 🔧 Core Features

**YAML Module (`yaml.ss`)**
- `yaml-string->json` - Parse YAML strings
- `yaml-file->json` - Load YAML files
- `yaml-available?` - Check if YAML support is installed

**Enhanced Graph Module (`charts/graph.ss`)**
- `graph-chart` - Accept JSON, YAML, or DOT input
- `graph-chart-from-file` - Auto-detect file format (.yaml, .json, .dot)

## File Format Comparison

### YAML (Easy to Read)
```yaml
type: digraph
name: MyProcess

nodes:
  - id: Start
    label: "Begin Here"
    fillcolor: lightgreen
  - id: End
    label: "All Done"
    fillcolor: lightblue

edges:
  - from: Start
    to: End
    label: "process"
```

### JSON (Machine-Friendly)
```json
{
  "type": "digraph",
  "name": "MyProcess",
  "nodes": [
    {"id": "Start", "label": "Begin Here", "fillcolor": "lightgreen"},
    {"id": "End", "label": "All Done", "fillcolor": "lightblue"}
  ],
  "edges": [
    {"from": "Start", "to": "End", "label": "process"}
  ]
}
```

YAML advantages:
- ✅ No quotes needed for most values
- ✅ Comments supported (`#`)
- ✅ Less punctuation (no braces, fewer commas)
- ✅ More readable indentation
- ✅ Multi-line strings easier

## YAML Features for Non-Technical Users

### 1. Comments Everywhere
```yaml
# This is a comment explaining what this section does
name: MyDiagram    # You can also put comments at the end of lines

# Comments help you remember what each part does
# Perfect for documenting your diagram structure
```

### 2. No Complicated Syntax
```yaml
# YAML - Simple and clean
label: My Process Step

# vs JSON - More punctuation
"label": "My Process Step"
```

### 3. Multi-line Labels
```yaml
# Easy multi-line text
label: "Step 1:\nValidate Input\nCheck Errors"

# Shows as:
# Step 1:
# Validate Input
# Check Errors
```

### 4. Clear Hierarchy
```yaml
clusters:
  - name: Stage1
    label: "First Stage"
    color: lightblue
    nodes:
      - id: A
      - id: B
```

## Prerequisites

YAML support requires Python 3 with PyYAML:

```bash
# Check if available
python3 -c "import yaml; print('YAML support OK')"

# If not installed
pip3 install pyyaml
```

## Usage Examples

### From Command Line
```bash
# Auto-detect format
gerbil-charts --yaml diagram.yaml output.svg

# Specify format
gerbil-charts --yaml diagram.yaml --format png output.png

# Different layout engine
gerbil-charts --yaml diagram.yaml --engine neato output.svg
```

### Programmatic Usage
```scheme
#!/usr/bin/env gxi
(import :gerbil-charts/charts/graph)

;; From YAML file (auto-detect)
(graph-chart-from-file "diagram.yaml"
                      output-file: "output.svg")

;; With options
(graph-chart-from-file "diagram.yaml"
                      engine: 'neato
                      format: 'png
                      output-file: "diagram.png")
```

## Customization Guide

### Colors
```yaml
# Named colors (easy)
fillcolor: lightgreen
fillcolor: lightblue
fillcolor: lightyellow
fillcolor: lightcoral

# Hex codes (precise)
fillcolor: "#e3f2fd"   # Very light blue
fillcolor: "#fff3e0"   # Light orange
fillcolor: "#e8f5e9"   # Light green
```

### Shapes
```yaml
shape: box        # Rectangle (default)
shape: oval       # Ellipse (start/end)
shape: diamond    # Decision points
shape: cylinder   # Databases
shape: folder     # File storage
shape: hexagon    # Connectors
```

### Layout
```yaml
graph:
  rankdir: TB    # Top to Bottom (default)
  rankdir: LR    # Left to Right
  rankdir: BT    # Bottom to Top
  rankdir: RL    # Right to Left
```

### Arrow Styles
```yaml
edges:
  - from: A
    to: B
    label: "success"
    color: green
    style: solid      # solid (default)
    style: dashed     # dashed line
    style: dotted     # dotted line
```

## For Non-Technical Users

**You don't need to know programming!** If you can:
- Edit a text file
- Follow examples
- Change labels and colors

...then you can create professional diagrams!

### Learning Path
1. ✅ Start with **QUICK_START.md** (5 minutes)
2. ✅ Copy and modify a template (15 minutes)
3. ✅ Reference **YAML_CHEATSHEET.md** as needed
4. ✅ Read **YAML_GUIDE.md** for detailed help

### Common Tasks

**Change text in a box:**
```yaml
label: "My New Text Here"
```

**Change a color:**
```yaml
fillcolor: lightblue    # or lightgreen, lightyellow, etc.
```

**Add a new step:**
```yaml
nodes:
  # ... existing nodes ...
  - id: NewStep
    label: "My New Step"
```

**Connect two boxes:**
```yaml
edges:
  - from: Step1
    to: Step2
```

## Validation

Common errors and fixes:

```yaml
# ✗ WRONG - No space after colon
name:MyDiagram

# ✓ CORRECT - Space after colon
name: MyDiagram

# ✗ WRONG - Tabs for indentation
    nodes:  # Uses tab

# ✓ CORRECT - Spaces for indentation
  nodes:  # Uses 2 spaces
```

## Testing

Test your YAML files:

```bash
# Run all examples
GERBIL_LOADPATH=./.gerbil/lib:$GERBIL_LOADPATH gxi examples/test-yaml.ss

# Test single file
gerbil-charts --yaml examples/beginner-flowchart.yaml test.svg
open test.svg
```

## Tips for Success

1. **Start Small** - Begin with 2-3 nodes, add more later
2. **Use Templates** - Don't start from scratch
3. **Test Often** - Generate diagram after each change
4. **Read Comments** - Templates explain every section
5. **Ask for Help** - Check the guides when stuck

## Real-World Examples

All 5 templates are production-ready examples:

1. **Approval Process** (beginner-flowchart.yaml)
   - Submit → Review → Approve/Reject → Complete
   - Shows decision diamonds, colored paths

2. **Data Pipeline** (data-pipeline-simple.yaml)
   - Source → Process → Output
   - 3 stages with color-coded clusters

3. **Team Structure** (organization-chart.yaml)
   - CEO → Department Heads → Team Members
   - Color-coded by level

4. **Shopping Flow** (website-flow.yaml)
   - Home → Products → Cart → Checkout → Confirmation
   - Shows user navigation paths

5. **Cloud Architecture** (network-diagram.yaml)
   - Users → Load Balancer → App Servers → Database
   - Network zones with proper shapes

## Next Steps

1. **Try a Template**: Copy and modify an example
2. **Create Your Own**: Use the cheat sheet
3. **Share**: Export to SVG/PNG/PDF
4. **Learn More**: Read the detailed guides

## Support

- **Quick Reference**: YAML_CHEATSHEET.md
- **Beginner Guide**: YAML_GUIDE.md
- **Quick Start**: QUICK_START.md
- **Examples**: examples/*.yaml

Happy diagramming! 🎨📊
