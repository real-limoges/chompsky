# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run

```bash
cabal build                                          # Build everything
cabal run chompsky -- --config-dir config parse-one   # Parse single text from stdin
cabal run chompsky -- --config-dir config dry-run -i FILE --limit N  # Parse N CSV rows, print JSON
cabal run chompsky -- --config-dir config backfill -i IN -o OUT -r REVIEWS  # Full batch with TUI
cabal run chompsky-trace -- --config-dir config [-i FILE]  # Per-stage NDJSON trace (stdin lines or CSV)
cabal test                                           # Run test suite
```

Input CSVs have `id` and `text` columns.

## Formatting

Uses **fourmolu** with project config in `fourmolu.yaml`: 4-space indentation, leading commas, 110-column limit, trailing function arrows.

```bash
fourmolu -i src/**/*.hs app/**/*.hs test/*.hs
```

## Architecture

Chompsky is a text extraction pipeline that parses structured data from natural-language remarks.
It has a **pure functional core** driven by **Lua configuration files**.

### Pipeline (Pure, No I/O)

Defined in `Pipeline.hs`, each stage transforms text through a functional chain:

1. **Normalize** (`Pipeline.Normalize`): lowercases, then expands abbreviations in two phases, multi-word phrases first (longest first), then single words with word-boundary guards.
2. **Clean** (`Pipeline.Clean`): strips phone numbers, deletes emails, collapses runs of `!`, `*` and `#` to one mark and whitespace runs to one space (or one newline, if the run held one), strips boilerplate phrases past `min_position`, and trims.
3. **Scanner** (`Pipeline.Scanner`): runs every parser entry against the cleaned text at each word start, and flags matches preceded by a negation word in the same sentence.
   Literals also match their `s` and `es` plurals.
   Parsers are built from specs in `Pipeline/Parser/Build.hs` on combinators from `Pipeline/Parser/Internal.hs`.
   Five strategies: `PhraseStrategy` (literal), `VerbalStrategy` (grammatical forms with optional years), `TriggerStrategy` (trigger then year), `MonetaryStrategy` (dollar amounts), `CaptureStrategy` (trigger then greedy capture).
4. **Triage** (`Pipeline.Triage`): flags rows for LLM review on long-no-signal, ambiguous phrases, high-vocab-no-extraction, conflicting tags or unrecognizable text, and sets `needsLlm`.
5. **Fuzzy** (`Pipeline.Fuzzy`): Mamdani fuzzy inference on feature count and triage-flag count, via the `hazy` library, gives a `Confidence` (High, Medium, Low) and a numeric score.

`processRemarkPure` runs the pipeline; `processRemarkTraced` runs it and keeps every stage's output, edit logs and scan hits, as a `ProcessTrace`.

### Key Types (`Types.hs`)

- `Extraction`: pipeline output, with the category map, confidence, triage details and LLM-review flag.
- `ExtractedValue`: `TagValue`, `TagWithYear`, `YearValue`, `AmountValue` or `CapturedText`.
- `CleanedText`: newtype wrapper for cleaned input.
- `InputRow`: CSV row with `rowId` and `rowText` (read from the `id` and `text` columns).
- `ProcessTrace`: per-stage snapshot for trace consumers, with `ScanHit`s (entry ID, `Span`, negation) and `TextEdit` logs (an `EditKind` plus a span).
- `FuzzySnapshot`: score and tier, as reported in a trace.

Edit spans are offsets in the text as rebuilt so far, so the logs replay in order.
Every Normalize and Clean substep must keep that true.

### Configuration (Lua)

All extraction rules live in `config/` as pure-data Lua files:

- `abbreviations.lua`: word and phrase replacement mappings.
- `boilerplate.lua`: phrases to strip, plus the `min_position` threshold.
- `triage.lua`: ambiguous phrases, vocabulary watch lists, conflicting tag pairs, thresholds.
- `parsers/*.lua`: one file per extraction category, each declaring a strategy and its entries.

Every file loads through `loadSandboxed` in `Config/Sandbox.hs`, which runs it with an empty `_ENV` (no globals, no standard library) under an instruction limit.
Every table is checked with `peekKnownKeys`, so a misspelled key fails the load, and a parser spec's `version` must be 1.
Parser specs are validated per strategy in `validateSpec` (`Config/ParserSpec.hs`); blank literals are rejected everywhere.
A new config key needs adding to its peeker's `peekKnownKeys` list as well as being read.

### I/O Layer

- `IO/CsvInput.hs`: streams the `id`, `text` CSV a row at a time (`streamInputCsv`), throwing `ChompskyError` on a parse failure.
- `IO/Csv.hs`: builds extraction and review rows, and `withCsvOutput` writes them a row at a time to `PATH.partial`, renaming it to `PATH` only when the run completes.
- `IO/Workers.hs`: `processStreaming` pre-builds scanner entries once, fully evaluates each row's result and hands it to a callback; it keeps nothing, so backfill memory stays flat.

### TUI (`TUI.hs`, `TUI/State.hs`, `TUI/Widgets.hs`)

Brick-based terminal UI for backfill mode.
The worker sends `BackfillEvent`s over an STM `TChan`; a bridge thread forwards them to Brick's `BChan`.
It shows phase, progress bar, rate, ETA, flagged count and confidence tier breakdown.
`BackfillFailed` halts it on a worker error.

### CLI (`CLI.hs`)

Three commands: `parse-one` (stdin to stdout), `dry-run` (CSV to stdout, no files) and `backfill` (CSV to CSV with the TUI).
`withErrorBoundary` catches `ChompskyError`, `IOException` and `Lua.Exception`, prints a labeled message and exits non-zero.
`runBackfill` re-raises a worker failure after the TUI halts, so backfill errors exit non-zero too.
The separate `chompsky-trace` executable (`app/Trace.hs`) writes one `ProcessTrace` JSON object per remark.

## Docs

`docs/reference/internal/` is committed and is the only part of `docs/` that code comments may cite.
It holds `design-decisions.md` (edit-log replay, sentence breaks in Clean, the fuzzy model, the Lua sandbox, negation, word boundaries and plurals) and `trace-schema.md` (the trace JSON).

## Dependencies

The `hazy` library is a `source-repository-package` in `cabal.project`, pinned to a commit on GitHub.
To build against a local checkout at `../hazy`, add `packages: . ../hazy` to a gitignored `cabal.project.local`.
