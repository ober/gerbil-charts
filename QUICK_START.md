# Quick Start Guide - YAML Diagrams

**Create professional diagrams in 3 simple steps!**

## Step 1: Choose Your Template

Pick the template that matches what you want to create:

| Template | Use For | Example |
|----------|---------|---------|
| `beginner-flowchart.yaml` | Approval processes, workflows | Request → Review → Approve/Reject |
| `data-pipeline-simple.yaml` | Data flows, ETL processes | Source → Process → Destination |
| `organization-chart.yaml` | Team structure, hierarchy | CEO → Managers → Team |
| `website-flow.yaml` | User journeys, navigation | Home → Product → Checkout |
| `network-diagram.yaml` | Infrastructure, cloud architecture | Users → Servers → Database |

## Step 2: Customize the Template

Open the template in any text editor and modify:

```yaml
# Change the title
name: MyProcess    # ← Put your diagram name here

# Change node labels
nodes:
  - id: Start
    label: "My Custom Text"    # ← Change what users see
    fillcolor: lightgreen      # ← Pick a color

# Change connections
edges:
  - from: Start
    to: Process
    label: "next step"         # ← Describe the connection
```

**What you can easily change:**
- ✏️ `label` - The text shown in boxes
- 🎨 `fillcolor` - Background colors (lightblue, lightgreen, etc.)
- 🔷 `shape` - Shape of boxes (box, oval, diamond, cylinder)
- ➡️ Arrow labels and colors

## Step 3: Generate Your Diagram

```bash
# From the gerbil-charts directory
gerbil-charts --yaml examples/beginner-flowchart.yaml my-diagram.svg

# Open and view
open my-diagram.svg
```

## Common Customizations

### Change Colors

```yaml
fillcolor: lightblue      # Predefined colors
fillcolor: "#e3f2fd"      # Hex codes for exact colors
```

**Popular colors:**
- `lightgreen` - Success, start
- `lightyellow` - Warning, pending
- `lightcoral` - Error, stop
- `lightblue` - Information, process

### Change Shapes

```yaml
shape: box        # Rectangle (default)
shape: oval       # Rounded (good for start/end)
shape: diamond    # Diamond (good for decisions)
shape: cylinder   # Cylinder (good for databases)
```

### Multi-line Labels

Use `\n` for line breaks:

```yaml
label: "Process Data\nStep 1 of 3"
```

### Change Direction

```yaml
graph:
  rankdir: TB    # Top to Bottom (vertical) - default
  rankdir: LR    # Left to Right (horizontal)
```

## Example: Simple 3-Step Process

Create `my-process.yaml`:

```yaml
type: digraph
name: MySimpleProcess

defaults:
  node:
    shape: box
    style: filled,rounded
    fillcolor: lightblue

nodes:
  - id: Start
    label: "Start Here"
    shape: oval
    fillcolor: lightgreen

  - id: Step1
    label: "Do This First"

  - id: Step2
    label: "Then Do This"

  - id: Done
    label: "All Done!"
    shape: oval
    fillcolor: lightgreen

edges:
  - from: Start
    to: Step1

  - from: Step1
    to: Step2

  - from: Step2
    to: Done
```

Generate it:

```bash
gerbil-charts --yaml my-process.yaml my-process.svg
```

## Troubleshooting

### Error: "YAML parsing error"

**Fix:** Check your spacing - use spaces not tabs, and put a space after colons:

```yaml
# ✓ Correct
name: MyDiagram

# ✗ Wrong
name:MyDiagram
```

### Error: "Node not found"

**Fix:** Make sure edge connections match node IDs exactly:

```yaml
nodes:
  - id: StartNode    # ← This name...

edges:
  - from: StartNode  # ← ...must match this
```

### Diagram looks cluttered

**Fix:** Try these settings:

```yaml
graph:
  rankdir: LR       # Try horizontal layout
  splines: ortho    # Use straight lines
```

## Next Steps

1. ✅ Read `YAML_GUIDE.md` for complete documentation
2. ✅ Explore all templates in `examples/` folder
3. ✅ Modify templates to match your needs
4. ✅ Share your diagrams!

## Command Reference

```bash
# Generate SVG (best for web/zoom)
gerbil-charts --yaml input.yaml output.svg

# Generate PNG (good for presentations)
gerbil-charts --yaml input.yaml --format png output.png

# Generate PDF (good for printing)
gerbil-charts --yaml input.yaml --format pdf output.pdf

# Use different layout engine
gerbil-charts --yaml input.yaml --engine neato output.svg
```

**Layout engines:**
- `dot` - Hierarchical (default, best for most cases)
- `neato` - Spring layout (good for networks)
- `circo` - Circular layout
- `fdp` - Force-directed (good for large graphs)

## Tips for Non-Technical Users

1. **Start with a template** - Don't start from scratch
2. **Make small changes** - Test often, change one thing at a time
3. **Use comments** - Add notes with `#` to remember what sections do
4. **Save versions** - Keep copies as you make changes
5. **Ask for help** - The examples have lots of comments explaining everything

Happy diagramming! 🎨📊
