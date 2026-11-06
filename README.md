# Chompsky

Domain-agnostic text extraction pipeline that parses structured data from natural-language remarks.

Chompsky runs a pure functional pipeline, driven by sandboxed Lua configuration files, to extract tagged values, years, dollar amounts and free text from unstructured input, with no ML.
All extraction rules are plain data files, so a new category or a new domain needs no Haskell changes.

## Quick Start

### Prerequisites

- GHC 9.12+
- Cabal
- Network access on the first build: the [`hazy`](https://github.com/real-limoges/hazy) fuzzy-logic library is pinned to a commit in `cabal.project` and fetched by Cabal.

To build against a local checkout of hazy at `../hazy` instead, create a `cabal.project.local` (gitignored) containing:

```
packages: . ../hazy
```

### Build

```bash
cabal build
```

### Run

**Parse a single remark** (stdin to JSON on stdout):

```bash
echo "acquired in 2019, maintenance cost \$450/month" | \
  cabal run chompsky -- --config-dir config parse-one
```

**Dry run** over a CSV file (print JSON, write no files; `--limit` defaults to 100):

```bash
cabal run chompsky -- --config-dir config dry-run -i data.csv --limit 20
```

**Backfill** a full CSV with a progress TUI:

```bash
cabal run chompsky -- --config-dir config backfill -i input.csv -o extractions.csv -r reviews.csv
```

**Trace** each stage of the pipeline as NDJSON, one object per remark:

```bash
echo "call 555-123-4567 re: qty pcs of electronics" | cabal run chompsky-trace -- --config-dir config
cabal run chompsky-trace -- --config-dir config -i input.csv > trace.ndjson
```

Input CSVs have a header row with two columns, `id` and `text`.
A leading byte-order mark is ignored.
`--config-dir` defaults to `config`.

## Pipeline Overview

Each remark passes through five pure stages:

1. **Normalize:** lowercases the text and expands abbreviations in two phases, multi-word phrases first (longest first), then single words with word-boundary guards.
2. **Clean:** strips phone numbers, deletes email addresses, collapses repeated punctuation and whitespace (keeping line breaks), strips boilerplate phrases past a `min_position` offset, and trims.
3. **Scan:** runs every parser entry against the cleaned text using one of five strategies (see Configuration), and detects negation within the same sentence.
4. **Triage:** flags rows for LLM review when a rule fires: long text with no signal, ambiguous phrases, high vocabulary with no extraction, conflicting tags, or unrecognizable text.
5. **Fuzzy:** Mamdani fuzzy inference over feature count and triage-flag count gives a confidence tier (high, medium, low) and a numeric score.

Normalize and Clean also return an ordered edit log, and the scanner returns every match with its span; `chompsky-trace` emits all of it.
The trace format and the reasoning behind the trickier pipeline rules are in [`docs/reference/internal/`](docs/reference/internal/).

## Configuration

All extraction rules live in `config/` as pure-data Lua files.
Every file runs in an empty environment, with no globals and no standard library, so it can only `return` a table.
Loading is strict: an unknown key, a blank phrase, a parser spec `version` other than 1, or a file that runs too long (a loop) fails with the file's path.
Abbreviation, boilerplate and triage entries are lowercased to match the lowercased input.

### Config Files

| File | Purpose |
|------|---------|
| `abbreviations.lua` | Word and phrase replacement mappings for normalization |
| `boilerplate.lua` | Phrases to strip during cleaning, with a `min_position` threshold |
| `triage.lua` | Ambiguous phrases, vocabulary watch lists, conflicting tag pairs, thresholds |
| `parsers/*.lua` | One file per extraction category, each declaring a strategy and entries |

### Parser Strategies

Each parser file in `parsers/` declares a `strategy` and a list of `entries`.
Literals match case-insensitively, longest first, and only on word boundaries.
A literal ending in a letter also matches its plural with `s` or `es`, so list singulars; irregular plurals must be listed.
The five strategies are:

**`phrase`:** literal phrase matching.
Each entry lists phrases to match against the text.

```lua
-- parsers/category.lua
return {
  version = 1, category = "category", strategy = "phrase",
  entries = {
    { tag = "Electronics", phrases = { "consumer electronics", "electronics" } },
    { tag = "Furniture",   phrases = { "office furniture", "furniture" } },
  },
}
```

**`verbal`:** grammatical verb forms applied to subjects, with an optional year.
Supports `action_verbs`, `suffix_verbs`, `adverbs`, `direct_phrases` and `suffix_extra`.

```lua
-- parsers/activity.lua
return {
  version = 1, category = "activity", strategy = "verbal",
  adverbs      = { "recently", "completely", "fully" },
  action_verbs = { "inspected", "audited", "reviewed" },
  suffix_verbs = { "inspected", "audited" },
  entries = {
    { tag = "Inventory", synonyms = { "inventory", "stock" },
      direct_phrases = { "audited inventory" } },
  },
}
```

**`trigger`:** a 4-digit year (1900 through 2099) following a trigger phrase.

```lua
-- parsers/date_acquired.lua
return {
  version = 1, category = "date_acquired", strategy = "trigger",
  entries = {
    { tag = "AcquisitionYear",
      triggers = { "acquired in ", "purchased in ", "received in " } },
  },
}
```

**`monetary`:** a dollar amount after a label phrase, with optional `bridges` (filler words between label and amount) and `frequency` suffixes.

```lua
-- parsers/cost.lua
return {
  version = 1, category = "cost", strategy = "monetary",
  entries = {
    { tag = "MaintenanceCost",
      labels    = { "maintenance fee ", "maintenance cost " },
      bridges   = { "is ", "of ", "approximately " },
      frequency = { "/month", " per month", " annually" } },
  },
}
```

**`capture`:** greedy text capture from a trigger phrase up to a terminator character.

```lua
-- parsers/note.lua
return {
  version = 1, category = "note", strategy = "capture",
  entries = {
    { tag = "HandlingNote",
      terminators = ".!\n;",
      triggers = { "special instructions", "handle with care", "fragile" } },
  },
}
```

## Output

Each extraction is a JSON object with:

| Field | Description |
|-------|-------------|
| `categories` | Map of category name to list of extracted values |
| `parser_confidence` | `"high"`, `"medium"` or `"low"` |
| `confidence_score` | Numeric fuzzy inference score, from 0.0 to 1.0 |
| `needs_llm` | True when any triage rule fired |
| `llm_reason` | Comma-joined tags of the triage rules that fired, or null |
| `triage_details` | The triage rules that fired, with context |

Extracted values are one of `tag`, `tag_with_year`, `year`, `amount` or `captured_text`.

In **backfill** mode two CSVs are written: an extractions file with every row (`parsed_at` is an ISO-8601 timestamp), and a reviews file with only the rows flagged for LLM review (header only when none are).
Rows stream through one at a time, so memory stays flat on large inputs.
Each file is written to `FILE.partial` and renamed into place only when the run completes.
If the worker fails, or the TUI is quit before it finishes, backfill removes the partial files, leaves any earlier output untouched, prints the reason and exits non-zero.

## Development

Format with [fourmolu](https://github.com/fourmolu/fourmolu) (project config in `fourmolu.yaml`):

```bash
fourmolu -i src/**/*.hs app/**/*.hs test/*.hs
```

Run the test suite:

```bash
cabal test
```
