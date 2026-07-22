#!/usr/bin/env bash
# scripts/tools/migrate_pilot_to_subtree.sh
#
# One-time migration: move the pilot code into scripts/pilot/ and rewrite the
# internal source() paths so it still runs from the new location.
#
# Keeps everything IN version control (uses `git mv`, which preserves file
# history) -- the pilot scripts are part of the methods provenance.
#
# Safe: touches only scripts/phase1, scripts/phase2, scripts/pilot_v3.
# Never touches scripts/production/ or scripts/parser.R.
#
# Run once, from the project root:
#   bash scripts/tools/migrate_pilot_to_subtree.sh

set -euo pipefail

if [ ! -d .git ]; then
  echo "ERROR: run this from the project root (no .git here)." >&2; exit 1
fi
if [ ! -d scripts/production ]; then
  echo "ERROR: scripts/production/ not found -- wrong directory?" >&2; exit 1
fi

# in-place sed that works on both macOS (BSD) and Linux (GNU)
sedi() { if sed --version >/dev/null 2>&1; then sed -i "$@"; else sed -i '' "$@"; fi; }

mkdir -p scripts/pilot

move_dir() {                      # move_dir <src> <dest>
  local src="$1" dest="$2"
  if [ ! -d "$src" ]; then echo "  skip (absent): $src"; return 0; fi
  if [ -e "$dest" ]; then echo "  skip (dest exists): $dest"; return 0; fi
  if git ls-files --error-unmatch "$src" >/dev/null 2>&1; then
    git mv "$src" "$dest"; echo "  git mv $src -> $dest  (history preserved)"
  else
    mv "$src" "$dest";     echo "  mv     $src -> $dest  (was untracked)"
  fi
}

echo "Moving pilot directories:"
move_dir scripts/phase1   scripts/pilot/phase1
move_dir scripts/phase2   scripts/pilot/phase2
move_dir scripts/pilot_v3 scripts/pilot/pilot_v3

echo
echo "Rewriting source()/path references inside scripts/pilot/ ..."
# Order matters: rewrite the OLD paths only. "scripts/production/phase1/" does
# not contain the substring "scripts/phase1/", so production refs are untouched.
while IFS= read -r -d '' f; do
  sedi \
    -e 's|scripts/phase1/|scripts/pilot/phase1/|g' \
    -e 's|scripts/phase2/|scripts/pilot/phase2/|g' \
    -e 's|scripts/pilot_v3/|scripts/pilot/pilot_v3/|g' \
    "$f"
done < <(find scripts/pilot -type f \( -name '*.R' -o -name '*.Rmd' \) -print0)
echo "  done."

echo
echo "Verification (excluding scripts/tools, which contains these patterns literally):"
stale=$(grep -rn 'scripts/phase[12]/' scripts/ --exclude-dir=tools 2>/dev/null || true)
if [ -n "$stale" ]; then
  echo "  !! stale old-path references remain:"; echo "$stale"
else
  echo "  OK: no stale scripts/phase1|2 references"
fi

if grep -rq 'scripts/pilot/production' scripts/ --exclude-dir=tools 2>/dev/null; then
  echo "  !! ERROR: a production path was rewritten -- inspect immediately."
else
  echo "  OK: scripts/production/ references untouched"
fi

if grep -rq '"scripts/parser.R"' scripts/ --exclude-dir=tools 2>/dev/null; then
  echo "  OK: scripts/parser.R references intact (shared by pilot + production)"
fi

echo
echo "Resulting layout:"
find scripts -maxdepth 2 -type d | sort | sed 's|^|  |'
echo
echo "Next: review with 'git status', then commit. Verify both trees still load:"
echo "  Rscript -e 'options(oscn.autorun.suppress=TRUE); source(\"scripts/production/phase1/pipeline.R\"); cat(\"production OK\\n\")'"
echo "  Rscript -e 'options(oscn.autorun.suppress=TRUE); source(\"scripts/pilot/phase2/classify_lfo.R\"); cat(\"pilot OK\\n\")'"
