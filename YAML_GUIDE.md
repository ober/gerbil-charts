# YAML Guide for Non-Technical Users

**Create beautiful diagrams without programming!**

This guide shows you how to create professional diagrams using simple text files. No coding knowledge required!

## Table of Contents
1. [What is YAML?](#what-is-yaml)
2. [Quick Start](#quick-start)
3. [Basic Concepts](#basic-concepts)
4. [Common Diagram Types](#common-diagram-types)
5. [Customization Guide](#customization-guide)
6. [Tips & Tricks](#tips--tricks)
7. [Troubleshooting](#troubleshooting)

## What is YAML?

YAML is a simple text format that's easy for humans to read and write. Think of it like filling out a form - you just need to follow the structure and fill in your own information.

**Example:**
```yaml
# This is a comment - it's ignored by the computer
name: My Diagram      # This is a setting
color: blue           # Another setting
```

## Quick Start

### 1. Choose a Template

We've created ready-to-use templates in the `examples/` folder:

- **`beginner-flowchart.yaml`** - Simple approval process
- **`data-pipeline-simple.yaml`** - Data flow between systems
- **`organization-chart.yaml`** - Team hierarchy
- **`website-flow.yaml`** - User navigation
- **`network-diagram.yaml`** - Infrastructure layout

### 2. Copy and Modify

```bash
# Copy a template
cp examples/beginner-flowchart.yaml my-diagram.yaml

# Open in your favorite text editor
nano my-diagram.yaml
# or
code my-diagram.yaml
```

### 3. Generate Your Diagram

```bash
# Create an SVG diagram
gerbil-charts --yaml my-diagram.yaml output.svg

# Or create a PNG
gerbil-charts --yaml my-diagram.yaml --format png output.png
```

## Basic Concepts

### Structure of a YAML Diagram

Every diagram has these main parts:

```yaml
type: digraph          # Type of diagram (digraph = arrows point one way)
name: MyDiagram        # Name of your diagram

graph:                 # Overall diagram settings
  rankdir: TB          # Direction: TB=Top-Bottom, LR=Left-Right

defaults:              # Default styling
  node:                # Default for all boxes/shapes
    fillcolor: white
  edge:                # Default for all arrows
    color: black

nodes:                 # Your boxes/shapes
  - id: Box1           # Unique identifier
    label: "Step 1"    # What users see

edges:                 # Your connections/arrows
  - from: Box1
    to: Box2
    label: "next"
```

### Understanding Nodes (Boxes/Shapes)

Nodes are the boxes, circles, or shapes in your diagram:

```yaml
nodes:
  - id: Start              # Must be unique (no spaces!)
    label: "Click Here"    # What users see (can have spaces)
    shape: oval            # Shape of the node
    fillcolor: lightgreen  # Background color
```

**Common Shapes:**
- `box` - Rectangle (default)
- `oval` - Oval/ellipse (good for start/end)
- `diamond` - Diamond (good for decisions)
- `cylinder` - Cylinder (good for databases)
- `folder` - Folder shape
- `hexagon` - Six-sided shape

**Common Colors:**
- `lightgreen`, `lightblue`, `lightyellow`, `lightcoral`
- `white`, `gray`, `lightgray`
- Or use hex codes: `#ff9999` (light red), `#99ccff` (light blue)

### Understanding Edges (Arrows/Connections)

Edges connect your nodes:

```yaml
edges:
  - from: Start        # Where the arrow starts
    to: Process        # Where the arrow points
    label: "begin"     # Text on the arrow (optional)
    color: green       # Arrow color (optional)
    style: dashed      # Arrow style (optional)
```

**Arrow Styles:**
- `solid` - Normal line (default)
- `dashed` - Dashed line
- `dotted` - Dotted line
- `bold` - Thick line

### Using Clusters (Groups)

Clusters group related nodes together:

```yaml
clusters:
  - name: TeamA               # Must start with lowercase letter
    label: "Team A"           # What users see
    color: "#e3f2fd"          # Background color
    nodes:                    # Nodes in this group
      - id: Person1
        label: "John"
      - id: Person2
        label: "Jane"
```

## Common Diagram Types

### 1. Flowchart (Process Flow)

**Use for:** Approval processes, workflows, decision trees

**Template:** `examples/beginner-flowchart.yaml`

**Key elements:**
- `oval` shapes for start/end
- `diamond` shapes for decisions
- `box` shapes for actions
- Use YES/NO labels on decision arrows

### 2. Data Pipeline

**Use for:** ETL processes, data flows, system integration

**Template:** `examples/data-pipeline-simple.yaml`

**Key elements:**
- `clusters` to group stages
- `cylinder` shapes for databases
- Consistent colors per stage
- Label arrows with data types

### 3. Organization Chart

**Use for:** Team structures, reporting relationships

**Template:** `examples/organization-chart.yaml`

**Key elements:**
- `rankdir: TB` (top to bottom)
- `splines: ortho` (straight lines)
- `arrowhead: none` (no arrows)
- Different colors per level

### 4. User Flow

**Use for:** Website navigation, app wireflows

**Template:** `examples/website-flow.yaml`

**Key elements:**
- `oval` for entry/exit points
- Different colors for page types
- `dashed` edges for "back" navigation
- Color-code success (green) vs error (red)

### 5. Network Diagram

**Use for:** Infrastructure, cloud architecture

**Template:** `examples/network-diagram.yaml`

**Key elements:**
- `clusters` for network zones
- `cylinder` for databases
- `folder` for storage
- `splines: ortho` for clean lines

## Customization Guide

### Changing Direction

```yaml
graph:
  rankdir: TB    # Top to Bottom (vertical)
  # OR
  rankdir: LR    # Left to Right (horizontal)
  # OR
  rankdir: BT    # Bottom to Top
  # OR
  rankdir: RL    # Right to Left
```

### Adding Notes/Comments

```yaml
# This is a comment - use for notes to yourself
# Comments start with #

nodes:
  - id: Process
    label: "Process Data"    # This also works for inline comments
```

### Multi-line Labels

Use `\n` for line breaks:

```yaml
nodes:
  - id: Server
    label: "Web Server\nPort 8080\n(Production)"
```

### Custom Colors

```yaml
# Named colors (easy to remember)
fillcolor: lightblue
fillcolor: lightyellow
fillcolor: lightcoral
fillcolor: lightgreen

# Hex colors (more precise)
fillcolor: "#e3f2fd"    # Very light blue
fillcolor: "#fff3e0"    # Very light orange
fillcolor: "#e8f5e9"    # Very light green
```

### Edge Styling

```yaml
edges:
  - from: A
    to: B
    label: "success"
    color: green        # Arrow color
    style: solid        # Line style
    penwidth: 2         # Thickness (1-5)
```

## Tips & Tricks

### 1. Start Simple

Begin with just a few nodes and edges. Add complexity gradually.

```yaml
# Start here
nodes:
  - id: A
    label: "Start"
  - id: B
    label: "End"

edges:
  - from: A
    to: B
```

### 2. Use Consistent Naming

Pick a naming style and stick to it:

```yaml
# Good - CamelCase
id: ProcessData
id: SendEmail

# Also Good - snake_case
id: process_data
id: send_email

# Bad - Inconsistent
id: ProcessData
id: send_email
```

### 3. Color Coding

Use colors to show:
- Status: Green (good), Yellow (warning), Red (error)
- Groups: Same color for related items
- Priority: Darker = more important

### 4. Keep Labels Short

Long labels make diagrams cluttered:

```yaml
# Too long
label: "This is the process that validates all user input"

# Better
label: "Validate\nUser Input"

# Even better
label: "Validate"
```

### 5. Test Incrementally

Generate your diagram often to see how changes look:

```bash
# Quick preview
gerbil-charts --yaml my-diagram.yaml test.svg && open test.svg
```

## Troubleshooting

### "YAML parsing error"

**Problem:** Syntax error in your YAML file

**Solutions:**
- Check that colons have a space after them: `name: value` ✓ not `name:value` ✗
- Check indentation (use spaces, not tabs)
- Make sure quotes match: `"text"` or `'text'`
- Verify all brackets match: `[ ]`

### "Node not found"

**Problem:** An edge references a node that doesn't exist

**Solution:**
```yaml
# Make sure this...
edges:
  - from: Start    # "Start" must exist in nodes

# ...matches this:
nodes:
  - id: Start      # Same name!
```

### Diagram looks messy

**Solutions:**
- Try different `rankdir` values (TB, LR, BT, RL)
- Add `splines: ortho` for straighter lines
- Reduce the number of cross-cluster connections
- Group related nodes into clusters

### Colors not showing

**Solutions:**
- Check spelling: `lightblue` not `light blue`
- Use quotes for hex colors: `"#ff9999"`
- Make sure `style` includes `filled`: `style: filled,rounded`

### Arrows pointing wrong way

**Solutions:**
- For org charts, use `arrowhead: none`
- Swap `from` and `to` if backwards
- Check `rankdir` - LR puts "from" on left, TB puts "from" on top

## Practice Exercise

Try modifying `examples/beginner-flowchart.yaml`:

1. Change "Submit Request" to your own process
2. Add a new decision point
3. Change the colors to match your preferences
4. Add a cluster to group related steps

## Need Help?

- Check the examples in `examples/` folder
- Look at `GRAPHVIZ_INTEGRATION.md` for advanced features
- See `README.md` for command-line options

## Quick Reference Card

```yaml
# Minimal working example
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

**Remember:**
- `id` = computer name (no spaces)
- `label` = human name (spaces OK)
- Use 2 spaces for indentation
- Start lists with `-`
- Comments start with `#`

Happy diagramming! 🎨
