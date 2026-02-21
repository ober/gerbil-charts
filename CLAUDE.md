# gerbil-charts

## Pre-Push Checklist (MANDATORY)

Before pushing ANY changes to GitHub, you MUST:

1. **Build modules**: `gerbil build` (or use `gerbil_build_and_report`)
2. **Run tests**: `gerbil test ./...` (or use `gerbil_run_tests` with directory mode)
3. **Build static binary**: `make static` — this builds a fully static Linux binary via Docker
4. **Test the static binary**: `.gerbil/bin/gcharts --help` to verify it runs

All four steps must pass before pushing. Do NOT push if any step fails.
