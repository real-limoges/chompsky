# Internal References

Written here and committed: architecture diagrams and pages that say how the system fits together, with enough prose to read them.

**A file here describes `main`.**
When the design changes, rewrite it in place, with no dated changelog and no correction paragraphs.

Nothing here cites the private parts of `docs/`.
Decision records may point at these pages; these pages state the facts themselves rather than pointing back.

Other material written here about how the system works (guides, explainers, walkthroughs) is not reference and is not committed; it stays in private notes.

| File | Shows |
|---|---|
| [`design-decisions.md`](design-decisions.md) | Load-bearing pipeline decisions: edit-log replay, the fuzzy model, the Lua sandbox, negation, word boundaries |
| [`trace-schema.md`](trace-schema.md) | The per-stage JSON that `chompsky-trace` emits and how to replay its edits |
