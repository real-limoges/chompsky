# Trace Output Schema

`chompsky-trace` writes one JSON object per remark (NDJSON), carrying a snapshot after every pipeline stage.
This shape is the contract the fugue `/parse` viz and the garcon `/parse/trace` endpoint consume.
It is pinned by the "ProcessTrace JSON schema" tests in `test/Main.hs`.

## Producing a Trace

As a library call, `processRemarkTraced :: AppConfig -> Text -> ProcessTrace` in `Chompsky.Pipeline` returns the trace, and its `ToJSON` instance (in `Types.hs`) produces the object below.
`processRemarkTracedWith` does the same with prebuilt scanner entries, for callers that trace many remarks.
`processRemarkPure` is the untraced path and stays available alongside it.

From the shell:

**Shell**

```bash
echo "call 555-123-4567 re: qty pcs of electronics" | cabal run chompsky-trace -- --config-dir config
cabal run chompsky-trace -- --config-dir config -i remarks.csv > tape.ndjson
```

`--config-dir` defaults to `config`.
Without `-i`, remarks are read from stdin, one per line, skipping blank lines; with `-i FILE`, the file is a CSV with `id` and `text` columns.

## Top-Level Object

| Key | Type | Content |
|---|---|---|
| `input` | String | The remark as given |
| `normalized` | String | Normalize output (lowercased, abbreviations expanded) |
| `normalize_edits` | Array of edits | How Normalize got from `input` to `normalized` |
| `cleaned` | String | Clean output, the text the scanner sees |
| `clean_edits` | Array of edits | How Clean got from `normalized` to `cleaned` |
| `scanned` | Extraction | Scanner output, before triage |
| `scan_hits` | Array of hits | Every scanner match, negated ones included |
| `triaged` | Extraction | Final output, with triage flags and confidence |
| `fuzzy` | Object | `score` (number) and `confidence` (`"high"`, `"medium"` or `"low"`) |

Confidence is computed inside triage, so `scanned` carries the defaults (`parser_confidence` `"low"`, `confidence_score` 0) and `triaged` and `fuzzy` carry the real values.
Read confidence from `fuzzy`.

## Edits

```
{ "kind": "abbrev_word", "span": { "start": 22, "end": 25 },
  "before": "qty", "after": "quantity", "detail": "qty" }
```

| Key | Content |
|---|---|
| `kind` | One of `abbrev_word`, `abbrev_phrase`, `phone_stripped`, `email_masked`, `punct_collapsed`, `whitespace_collapsed`, `boilerplate_stripped` |
| `span` | Half-open character range of `before`, in the text as it stands just before this edit (see Replaying Edits) |
| `before` | The text replaced |
| `after` | The replacement; `""` for a deletion |
| `detail` | The abbreviation key or boilerplate phrase, or `null` |

`email_masked` deletes the address (`after` is `""`); the name predates that and is kept for compatibility.
A phone number is replaced by a single space, which the whitespace pass then collapses.
A run of two or more `!`, `*` or `#` becomes its first character; a single mark is not edited.
A whitespace run becomes one `"\n"` if it holds a newline and one space otherwise, so `cleaned` can contain newlines.
The final trim of leading and trailing whitespace appears as `whitespace_collapsed` edits with `after` `""`.

## Replaying Edits

Each log replays to its stage output exactly when applied in order, each edit replacing `span` with `after`:

- `normalize_edits` replays from `input` lowercased to `normalized`.
- `clean_edits` replays from `normalized` lowercased to `cleaned`.

Spans are character offsets (Unicode code points), not bytes.
They are in lowercased text, and lowercasing can lengthen a string (`İ` becomes two code points), so a span is not guaranteed to index `input` itself.

## Scan Hits

```
{ "entry_id": "category/Electronics", "category": "category",
  "value": { "type": "tag", "value": "Electronics" },
  "span": { "start": 28, "end": 39 },
  "is_negated": false, "negation_word": null }
```

| Key | Content |
|---|---|
| `entry_id` | `"<category>/<tag>"` for the config entry that fired; a repeated tag within one spec file gets `#2`, `#3` and so on |
| `category` | The category the value goes into |
| `value` | An extracted value (below) |
| `span` | Half-open character range of the match in `cleaned` |
| `is_negated` | True when a negation word sits within three words before the match, in the same sentence |
| `negation_word` | That word, or `null` |

Negated hits appear here but are not merged into `scanned` or `triaged`.
Building `scanned.categories` from this list is a pure fold: drop negated hits, then collect each category's values as a sorted list without duplicates.

## Extraction and Values

An extraction has `categories` (an object from category name to an array of values), `needs_llm`, `llm_reason` (comma-joined triage rule tags, or `null`), `triage_details`, `parser_confidence` and `confidence_score`.

| `type` | Other Keys |
|---|---|
| `tag` | `value`: the tag |
| `tag_with_year` | `value`: the tag; `year`: a number or `null` |
| `year` | `value`: a number |
| `amount` | `value`: a number |
| `captured_text` | `value`: the captured text |

Each triage detail carries a `rule` tag plus the fields of that rule:

| `rule` | Fires When |
|---|---|
| `long_remarks_no_signal` | A long remark yields no extraction |
| `ambiguous_phrase` | A configured ambiguous phrase appears |
| `high_vocab_no_extraction` | Enough watch-list words appear in a category that extracted nothing |
| `conflicting_signals` | A category holds tags from both sides of a configured conflict pair |
| `unrecognizable_text` | The share of non-ASCII characters passes a threshold (likely garbled text) |

Before 2026-11-06 the second and third tags were `ambiguous_condition` and `complex_renovation_narrative`.
Output uses only the new tags; decoding still accepts the old ones, so older tapes load.
