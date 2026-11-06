# Design Decisions

Subtle, load-bearing decisions in the Chompsky pipeline that are easy to break and hard to re-derive from the code alone.
Each section states the decision, why it is that way, and the code that depends on it.
Code comments point here (by section title) instead of restating the rationale.

## Edit-Log Replay Invariant

The Normalize and Clean stages each return an ordered `[TextEdit]` alongside their transformed text.
Every edit's span locates its `before` text in the text as it stands just before that edit is applied, that is, after every earlier edit in the same stage's log.

The trace consumers (the fugue `/parse` viz) rely on this to animate a stage one change at a time.
A consumer starts from the stage's input, applies each edit in order by replacing `span` with `after`, and arrives at the stage's output exactly:

- Normalize replays from `T.toLower` of the raw input to `normalized`.
- Clean replays from `T.toLower` of `normalized` to `cleaned`.

If a span were measured in any other text (the raw input, or the input to the substep that emitted it), the second edit in a substep would point at a stale offset as soon as the first one changed the length, and the replay would drift.

Every substep honors the contract the same way: it walks its input once, builds its output in an accumulator, and keeps a position counter that advances by what it **emits**, not by what it consumes.
An edit is stamped with that counter, which is its offset in the text as rebuilt so far.

- `normalize` in `Pipeline/Normalize.hs` runs phrase replacements (`replacePhraseAll`, which keeps `outPos` apart from the input it is consuming) and then word replacements (`applyWordReplacements`, which threads `outPos` through its fold).
- `clean` in `Pipeline/Clean.hs` runs `stripPhoneNumbers`, `stripEmails`, `stripRepeatedPunctuation`, `normalizeWhitespace`, `stripBoilerplate` and `trimEdges`, in that order.
- `trimEdges` logs the final strip as up to two `WhitespaceCollapsed` edits (leading, then trailing), so the replay ends on `cleaned` exactly.
- The `teSpan` field doc in `Types.hs` states the contract for the type.
- The "edit-log replay" tests in `test/Main.hs` replay both logs on fixed inputs and compare against the stage outputs.

Spans are in lowercased text.
`T.toLower` can lengthen a string (`İ` becomes two code points), so a span is not guaranteed to index the raw `input` field of a trace.

## Clean Keeps Sentence Breaks

Clean keeps the line structure the capture strategy depends on.
`normalizeWhitespace` collapses a whitespace run to a single `"\n"` when the run holds a newline, and to a single space otherwise.
`stripRepeatedPunctuation` collapses a run of two or more `!`, `*` or `#` to its first character and leaves a single mark alone.
Both are needed for a capture's `"\n"` and `"!"` terminators to still be there when the scanner runs, and the negation lookback stops at the same breaks.

Phone stripping (`stripPhoneNumbers`) takes three, three and four digits separated by nothing, a dash, a dot or one space, with the area code optionally in parentheses (`(555) 123-4567`).
A number inside a longer run of digits on either side is left alone.

## Fuzzy Confidence Model

Confidence is a Mamdani fuzzy inference over two integer inputs, not a hand-tuned formula.
It lives in `fuzzyConfidence` in `Pipeline/Fuzzy.hs` and runs on the `hazy` library.

Inputs, both clamped before inference:

- `featureCount`: number of extracted values, clamped to at most 10.
- `ambiguityCount`: number of triage rules that fired, clamped to at most 5.

The membership functions define the linguistic terms over those ranges:

| Variable | Term | Shape |
|---|---|---|
| Features | `few` | `trapezoidal (-1) 0 0 2` |
| Features | `some` | `triangular 1 3 5` |
| Features | `many` | `trapezoidal 4 6 10 11` |
| Ambiguity | `none` | `trapezoidal (-1) 0 0 1` |
| Ambiguity | `mild` | `triangular 0 1 3` |
| Ambiguity | `severe` | `trapezoidal 2 3 5 6` |
| Confidence | `low` | `triangular 0 0 0.4` |
| Confidence | `medium` | `triangular 0.2 0.5 0.8` |
| Confidence | `high` | `triangular 0.6 1.0 1.0` |

Hazy's `triangular` and `trapezoidal` return 0 at their outer feet (`x <= a` and `x >= d`).
A term that must be fully on at a bound of its variable therefore gets a shoulder that reaches past the bound.
Written as `triangular 0 0 c`, the `few` and `none` terms would be 0 at an input of 0, no rule would fire for an unambiguous row, and Hazy would fall back to the midpoint score of 0.5, so every such row would land in Medium.
The same applies at the top: `trapezoidal 4 6 10 10` is 0 at the clamp ceiling of 10.

The rule table is the full 3x3 cross of features against ambiguity:

- Some or many features with no ambiguity is high.
- The same with mild ambiguity is medium.
- Anything with severe ambiguity, and every case with few features, is low.

So "many features, severe ambiguity" scores low despite the features, because heavy ambiguity dominates, and a single extraction with no flags scores low because one feature is still "few".

The defuzzified score maps to a tier by two cutoffs: High at a score of 0.65 or more, Medium at 0.35 or more, Low below 0.35.
`fuzzyConfidence` is the one place these cutoffs live, as literals; the `Confidence` type in `Types.hs` points there rather than restating them.

## Sandboxed Lua Config Loading

Every config file is pure data and is loaded that way on purpose: `abbreviations.lua`, `boilerplate.lua`, `triage.lua` and each file under `parsers/` run with an empty global environment.
A config file cannot read globals, call a standard-library function or cause a side effect; one that tries to do more than `return { ... }` has nothing available to it and fails to load.

This keeps config strictly declarative and makes it safe to load from any directory, which matters because callers such as garcon are expected to own their own `config/` and pass it to `loadConfig`.

The mechanism is `loadSandboxed` in `Config/Sandbox.hs`, which both `loadConfig` (in `Config.hs`) and `loadParserSpecs` (in `Config/ParserSpec.hs`) call:

- `loadfile` compiles the chunk and leaves the compiled function on the Lua stack.
- `Lua.newtable` pushes a fresh empty table.
- `Lua.setupvalue (StackIndex (-2)) 1` pops that table into upvalue 1 of the chunk, which under Lua 5.4 is `_ENV`, the chunk's global environment.
  The index `-2` names the chunk (second from the top, just under the new table).
- `limitInstructions` installs a count hook that raises an error after `instructionLimit` (ten million) VM instructions.
  It opens the debug library only to reach `debug.sethook`, and never puts it where the chunk can see it.
  Real config files finish in a few hundred instructions; the limit exists so a file that loops fails instead of hanging the program.
- `pcall 0 1` runs the chunk, and the returned table is peeked and popped.

The shape of the returned table is checked strictly:

- Every table the loaders read, top level and per entry, goes through `peekKnownKeys`, which fails on any key it does not list.
  Most fields are optional, so without the check a misspelled key (`min_postion`, `bridge`) would be silently ignored.
- A parser spec's `version` must equal `supportedVersion` in `Config/ParserSpec.hs`, currently 1.

Two loader-level checks sit on top of the sandbox:

- Literals are rejected when blank: `validateSpec` rejects blank entries in every parser-spec list and a blank tag for every strategy, and `loadLuaFile` in `Config.hs` rejects blank abbreviation keys, boilerplate phrases and triage phrases and words.
  A blank literal matches the empty string at every position, and `T.breakOn ""` calls `error`.
- Abbreviation keys, boilerplate phrases and triage phrases and words are lowercased at load, because the pipeline lowercases its input and those lookups compare exactly.
  Parser-spec literals need no lowercasing: the scanner matches them with Megaparsec's case-insensitive `string'`.

Parser spec files load in file-name order, so the order of scan hits in a trace is deterministic.

## Negation: Scan Then Filter

The scanner detects negation (a match preceded within three words by a negation word such as "not" or "don't") but does not throw negated matches away.

`scanAll` returns both an `Extraction` and a flat list of every `ScanHit`, and negated hits stay in that list with `shIsNegated = True` and the word that negated them in `shNegationWord`.
They are dropped only when hits become an `Extraction`.
`scanAll` builds its `Extraction` with `hitsToExtraction`, the same function trace consumers can call on the hit list, so the two paths cannot disagree.
`hitsToExtraction` skips negated hits and keeps each category's values as a sorted, de-duplicated list; there is one merge policy, not one per entry.

The split keeps extraction behavior unaffected by negated matches while trace consumers still receive them, so the viz can render a negated match struck through instead of silently omitting it.

The lookback is `precedingWords` in `Pipeline/Scanner.hs`, called with a window of 3.
It stops at a sentence break: a newline, or a `.`, `;`, `?` or `!` followed by whitespace.
So "no damage. electronics" extracts Electronics, while "no tools, electronics" does not, because a comma does not end the window.
A decimal point (`$25.99`) is not a break, since no whitespace follows it.
The lookback only looks before the match, never after it, so "warehouse not inspected" is not read as negated; it simply fails to match, because "not" breaks the suffix form.

## Word-Boundary Matching Is Duplicated on Purpose

Several stages do their own boundary-aware matching rather than sharing one helper:

| Site | What It Guards | Boundary Test |
|---|---|---|
| `replacePhraseAll` (local `isWordBoundary`) in `Pipeline/Normalize.hs` | Phrase abbreviations | Neighbor is not `isAlphaNum` |
| `tokenize` in `Pipeline/Normalize.hs` | Word abbreviations | Words are runs of `isAlphaNum` or `/`, so `a/c` stays one word |
| `containsPhrase` in `Pipeline/Triage.hs` | Ambiguous-phrase detection | Neighbor is not `isAlphaNum` |
| `collectMatches` and `oneOfLits` in the scanner | Parser matches | A match starts after a non-`isAlphaNum` character; a literal ending in a letter or digit, and the match as a whole, must not be followed by one |
| `precedingWords` in `Pipeline/Scanner.hs` | Negation lookback | Words are runs of `isAlphaNum` or an apostrophe (straight or curly), so "don't" stays whole |

The policy they share is that a phrase matches only when its edges fall on word boundaries.
Only the negation lookback treats an apostrophe as part of a word, because it compares whole words against a list of contractions.

Boilerplate stripping (`stripBoilerplate` in `Pipeline/Clean.hs`) has no boundary check: boilerplate phrases are long multi-word sentences that do not occur inside other words in practice.

`oneOfLits` also accepts the plural of a literal that ends in a letter: the literal plus `s`, or plus `es`, still with the right boundary.
So config lists singulars, and "electronic device" matches "electronic devices" while "electronic devicesss" matches nothing.
Irregular plurals ("shelves") must still be listed.
The phrase-matching sites in Normalize and Triage do not do this; they match exactly.

The duplication is deliberate.
Each site matches against a different shape (a dictionary of single words, a list of phrases, Megaparsec parsers) and threads its own edit log or offset bookkeeping, so a shared combinator would need enough parameters that it would not be simpler.
A change to the boundary rule at one site should be checked against the others.
