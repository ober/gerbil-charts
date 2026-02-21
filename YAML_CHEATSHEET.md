# YAML Diagram Cheat Sheet

**One-page reference for creating diagrams**

## Minimal Example

```yaml
type: digraph
name: MyDiagram

nodes:
  - id: A
    label: "First"
  - id: B
    label: "Second"

edges:
  - from: A
    to: B
```

## Structure

```yaml
type: digraph              # Always use "digraph"
name: DiagramName          # Your diagram name (no spaces)

graph:                     # Diagram settings (optional)
  rankdir: TB              # TB, LR, BT, or RL
  fontname: Arial

defaults:                  # Default styling (optional)
  node:
    shape: box
    fillcolor: white
  edge:
    color: black

clusters:                  # Group nodes (optional)
  - name: GroupName
    label: "Group Label"
    color: lightgray
    nodes:
      - id: Node1
      - id: Node2

nodes:                     # Your boxes/shapes
  - id: NodeID             # Unique ID (no spaces!)
    label: "Display Text"  # What users see
    shape: box             # Shape
    fillcolor: lightblue   # Color

edges:                     # Connections/arrows
  - from: NodeID1
    to: NodeID2
    label: "arrow text"    # Optional
```

## Common Node Shapes

| Shape | Use For |
|-------|---------|
| `box` | Processes, steps (default) |
| `oval` | Start/end points |
| `diamond` | Decisions (yes/no) |
| `cylinder` | Databases, storage |
| `folder` | File storage |
| `hexagon` | Generic connector |

## Common Colors

| Color | Hex Code | Use For |
|-------|----------|---------|
| `lightgreen` | `#90EE90` | Success, start, go |
| `lightblue` | `#ADD8E6` | Information, process |
| `lightyellow` | `#FFFFE0` | Warning, review |
| `lightcoral` | `#F08080` | Error, stop |
| `white` | `#FFFFFF` | Neutral |
| `lightgray` | `#D3D3D3` | Inactive, background |

## Edge Styles

```yaml
edges:
  - from: A
    to: B
    label: "text"          # Label on arrow
    color: green           # Arrow color
    style: solid           # solid, dashed, dotted
    penwidth: 2            # Thickness (1-5)
```

## Direction

```yaml
graph:
  rankdir: TB    # Top → Bottom (vertical)
  rankdir: LR    # Left → Right (horizontal)
  rankdir: BT    # Bottom → Top
  rankdir: RL    # Right → Left
```

## Multi-line Text

```yaml
label: "Line 1\nLine 2\nLine 3"
```

## Comments

```yaml
# This is a comment
name: MyDiagram    # Inline comment
```

## Generate Commands

```bash
# SVG (scalable)
gerbil-charts --yaml file.yaml output.svg

# PNG (fixed size)
gerbil-charts --yaml file.yaml --format png output.png

# PDF (printable)
gerbil-charts --yaml file.yaml --format pdf output.pdf

# Different layout
gerbil-charts --yaml file.yaml --engine neato output.svg
```

## Common Patterns

### Simple Linear Flow
```yaml
A → B → C → D
```

### Decision Point
```yaml
nodes:
  - id: Question
    shape: diamond
edges:
  - from: Question
    to: YesPath
    label: "YES"
    color: green
  - from: Question
    to: NoPath
    label: "NO"
    color: red
```

### Loop Back
```yaml
edges:
  - from: End
    to: Start
    style: dashed
```

### Clustered Groups
```yaml
clusters:
  - name: Stage1
    label: "Stage 1"
    nodes:
      - id: A
      - id: B
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Parsing error | Check spacing after `:` |
| Node not found | Match `id` exactly in edges |
| No arrows | Check `from` and `to` IDs |
| Wrong direction | Try different `rankdir` |
| Messy layout | Add `splines: ortho` |

## Tips

✅ **DO:**
- Use consistent ID naming
- Add comments to explain sections
- Test after small changes
- Start with a template

❌ **DON'T:**
- Use spaces in IDs
- Forget quotes around labels with special characters
- Mix tabs and spaces
- Make IDs too long

---

**Quick Test:**
```bash
echo 'type: digraph
name: Test
nodes:
  - id: A
    label: "Hello"
  - id: B
    label: "World"
edges:
  - from: A
    to: B' > test.yaml

gerbil-charts --yaml test.yaml test.svg
open test.svg
```
