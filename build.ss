#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  '("data"
    "scale"
    "canvas"
    "canvas/svg"
    "canvas/cairo"
    "axis"
    "legend"
    "layout"
    "charts/line"
    "charts/bar"
    "charts/scatter"
    "charts/area"
    "charts"
    "prometheus"
    "dashboard"
    "main"
    (exe: "main" bin: "gerbil-charts")))
