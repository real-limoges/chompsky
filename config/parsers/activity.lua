-- VerbalStrategy: detects action phrases applied to subjects.
-- Required fields: version, category, strategy, entries,
--   adverbs, action_verbs, suffix_verbs (each a non-empty list)
-- Each entry requires: tag (string), synonyms (non-empty list)
-- Optional per-entry fields: direct_phrases, suffix_extra (extra subjects
--   accepted only in the suffix form, e.g. "machines" in "machines serviced")

return {
  version  = 1,
  category = "activity",
  strategy = "verbal",

  -- Adverbs that may precede action verbs (e.g. "recently inspected")
  adverbs      = { "recently", "completely", "fully", "partially", "thoroughly" },

  -- Verbs used in active constructions (e.g. "audited inventory")
  action_verbs = { "inspected", "audited", "reviewed", "cataloged", "verified", "sorted", "serviced" },

  -- Verbs used in passive/suffix constructions (e.g. "inventory audited")
  suffix_verbs = { "inspected", "audited", "reviewed", "cataloged", "verified", "serviced" },

  entries = {
    { tag = "Inventory",
      synonyms       = { "inventory", "stock" },
      direct_phrases = { "audited inventory", "reviewed stock" } },

    { tag = "Warehouse",
      synonyms       = { "warehouse", "storage facility" },
      direct_phrases = { "inspected warehouse" } },

    { tag = "Equipment",
      synonyms = { "equipment", "machinery" } },

    { tag = "Records",
      synonyms = { "records", "documentation", "files" } },
  },
}
