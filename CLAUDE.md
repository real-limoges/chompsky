# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run

```bash
cabal build                                          # Build everything
cabal run chompsky -- --config-dir config parse-one   # Parse single text from stdin
cabal run chompsky -- --config-dir config dry-run -i FILE --limit N  # Parse N CSV rows, print JSON
cabal run chompsky -- --config-dir config backfill -i IN -o OUT -r REVIEWS  # Full batch with TUI
cabal test                                           # Run test suite
```

## Formatting

Uses **fourmolu** with project config in `fourmolu.yaml`. Key settings: 2-space indentation, leading commas, 110-column limit, trailing function arrows.

```bash
fourmolu -i src/**/*.hs app/**/*.hs
```

## Architecture

Chompsky is a text extraction pipeline that parses structured data from natural language remarks. It has a **pure functional core** driven by **Lua configuration files**.

### Pipeline (pure, no I/O)

Defined in `Pipeline.hs`, each stage transforms text through a clean functional chain:

1. **Normalize** (`Pipeline.Normalize`) — Expands abbreviations via two-phase replacement: multi-word phrases first (longest-first), then single-word substitutions with word-boundary guards.
2. **Clean** (`Pipeline.Clean`) — Strips phone numbers, masks emails, removes repeated punctuation, normalizes whitespace, strips boilerplate phrases (with a `min_position` threshold to protect early content).
3. **Scanner** (`Pipeline.Scanner`) — Runs all parser entries against cleaned text. Five strategies: `PhraseStrategy` (literal), `VerbalStrategy` (grammatical forms with optional years), `TriggerStrategy` (trigger + year), `MonetaryStrategy` ($ amounts), `CaptureStrategy` (trigger + greedy capture). Handles negation detection.
4. **Triage** (`Pipeline.Triage`) — Post-extraction quality flagging. Detects: long-no-signal, ambiguous phrases, high-vocab-no-extraction, conflicting tags, unrecognizable text. Sets `needsLlm` flag when rules fire.
5. **Fuzzy** (`Pipeline.Fuzzy`) — Mamdani fuzzy inference on feature count + ambiguity to produce a `Confidence` (High/Medium/Low) and numeric score.

### Key Types (`Types.hs`)

- `Extraction` — Pipeline output: category map, confidence, triage details, LLM-review flag
- `ExtractedValue` — Sum type: `TagValue`, `TagWithYear`, `YearValue`, `AmountValue`, `CapturedText`
- `CleanedText` — Newtype wrapper for cleaned input
- `InputRow` — CSV row with `rowId` and `rowText`

### Configuration (Lua)

All extraction rules live in `config/` as pure-data Lua files (sandboxed, no stdlib):

- `abbreviations.lua` — Word and phrase replacement mappings
- `boilerplate.lua` — Phrases to strip + `min_position` threshold
- `triage.lua` — Ambiguous phrases, vocab watch lists, conflicting tag pairs, thresholds
- `parsers/*.lua` — One file per extraction category, each declaring a strategy and its entries

Parser specs are loaded via HsLua in `Config/ParserSpec.hs` and validated per-strategy.

### I/O Layer

- `IO/CsvInput.hs` — Reads 2-column CSV (rowId, rowText)
- `IO/Csv.hs` — Writes extraction and review CSVs
- `IO/Workers.hs` — Streaming processor: pre-builds scanner entries once, processes rows with a callback for TUI updates, collects results strictly via `NFData`

### TUI (`TUI.hs`, `TUI/State.hs`, `TUI/Widgets.hs`)

Brick-based terminal UI for backfill mode. Worker thread sends `BackfillEvent`s over STM `TChan`; a bridge thread forwards them to Brick's `BChan`. Displays phase, progress bar, rate, ETA, flagged count, and confidence tier breakdown.

### CLI (`CLI.hs`)

Three commands: `parse-one` (stdin→stdout), `dry-run` (CSV→stdout, no files), `backfill` (CSV→CSV with TUI). Error boundary catches `ChompskyError`, `IOException`, and `Lua.Exception`.

## Dependencies

The `hazy` library (sibling repo at `../hazy`) is included via `cabal.project` as an `optional-packages` source dependency.