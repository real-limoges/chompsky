# Chompsky

Domain-agnostic text extraction pipeline that parses structured data from natural language remarks.

Chompsky uses a pure functional pipeline driven by sandboxed Lua configuration files to extract tagged values, years, dollar amounts, and free text from unstructured input — no ML required. All extraction rules are plain data files, making it easy to add new categories or adapt to different domains without changing Haskell code.

## Quick Start

### Prerequisites

- GHC 9.12+
- Cabal
- The [`hazy`](../hazy) sibling library must be checked out at `../hazy` (included via `cabal.project` as a source dependency)

### Build

```bash
cabal build
```

### Run

**Parse a single remark** (stdin to JSON on stdout):

```bash
echo "acquired in 2019, maintenance cost $450/month" | \
  cabal run chompsky -- --config-dir config parse-one
```

**Dry run** over a CSV file (print JSON, no output files):

```bash
cabal run chompsky -- --config-dir config dry-run -i data.csv --limit 20
```

**Backfill** a full CSV with TUI progress display:

```bash
cabal run chompsky -- --config-dir config backfill -i input.csv -o extractions.csv -r reviews.csv
```

The input CSV has two columns: `rowId` and `rowText`.

## Pipeline Overview

Each remark passes through five pure stages:

1. **Normalize** — Expands abbreviations via two-phase replacement: multi-word phrases first (longest-first), then single-word substitutions with word-boundary guards.
2. **Clean** — Strips phone numbers, masks emails, removes repeated punctuation, normalizes whitespace, and strips boilerplate phrases (with a `min_position` threshold to protect early content).
3. **Scan** — Runs all parser entries against cleaned text using one of five strategies (see [Configuration](#configuration)). Handles negation detection.
4. **Triage** — Post-extraction quality flagging. Detects long-no-signal, ambiguous phrases, high-vocab-no-extraction, conflicting tags, and unrecognizable text. Sets `needsLlm` when rules fire.
5. **Fuzzy** — Mamdani fuzzy inference on feature count and ambiguity to produce a confidence tier (High/Medium/Low) and numeric score.

## Configuration

All extraction rules live in `config/` as pure-data Lua files. The Lua environment is sandboxed with no standard library — files are strictly declarative.

### Config files

| File | Purpose |
|------|---------|
| `abbreviations.lua` | Word and phrase replacement mappings for normalization |
| `boilerplate.lua` | Phrases to strip during cleaning, with `min_position` threshold |
| `triage.lua` | Ambiguous phrases, vocab watch lists, conflicting tag pairs, thresholds |
| `parsers/*.lua` | One file per extraction category, each declaring a strategy and entries |

### Parser strategies

Each parser file in `parsers/` declares a `strategy` and a list of `entries`. The five strategies are:

**`phrase`** — Literal phrase matching. Each entry lists phrases to match against the text.

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

**`verbal`** — Grammatical verb forms applied to subjects, with optional year extraction. Supports `action_verbs`, `suffix_verbs`, `adverbs`, `direct_phrases`, and `suffix_extra`.

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

**`trigger`** — Extracts a 4-digit year following a trigger phrase.

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

**`monetary`** — Extracts dollar amounts preceded by label phrases. Supports optional `bridges` (filler words) and `frequency` suffixes.

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

**`capture`** — Greedy text capture from a trigger phrase up to a terminator character.

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

Each extraction produces a JSON object with:

| Field | Description |
|-------|-------------|
| `categories` | Map of category name to list of extracted values |
| `parser_confidence` | `"high"`, `"medium"`, or `"low"` |
| `confidence_score` | Numeric fuzzy inference score (0.0–1.0) |
| `needs_llm` | Boolean flag set when triage rules fire |
| `llm_reason` | Optional text explaining why LLM review is needed |
| `triage_details` | List of triage rules that fired, with context |

Extracted values are one of: `tag`, `tag_with_year`, `year`, `amount`, or `captured_text`.

In **backfill** mode, two CSVs are written: an extractions file with all results and a reviews file containing only rows flagged for LLM review.

## Development

Format with [fourmolu](https://github.com/fourmolu/fourmolu) (project config in `fourmolu.yaml`):

```bash
fourmolu -i src/**/*.hs app/**/*.hs
```

Run the test suite:

```bash
cabal test
```
