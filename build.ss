#!/usr/bin/env gxi
(import :std/build-script
        :std/make)
(defbuild-script
  `("data"
    "scale"
    "canvas"
    "canvas/svg"
    "canvas/cairo"
    "axis"
    "legend"
    "layout"
    "yaml"
    "graphviz"
    "charts/line"
    "charts/bar"
    "charts/scatter"
    "charts/area"
    "charts/graph"
    "charts"
    "main"))
