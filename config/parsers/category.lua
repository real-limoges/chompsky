-- PhraseStrategy: matches literal phrases to classify items into category tags.
-- Required fields: version, category, strategy, entries
-- Each entry requires: tag (string), phrases (non-empty list of strings)
-- Phrases may be listed in any order: the scanner tries the longest first.

return {
  version  = 1,
  category = "category",
  strategy = "phrase",
  entries = {
    { tag = "Electronics", phrases = { "consumer electronics", "electronic device", "electronics" } },
    { tag = "Furniture",   phrases = { "office furniture", "furniture" } },
    { tag = "Clothing",    phrases = { "clothing", "apparel", "garments" } },
    { tag = "Perishable",  phrases = { "perishable goods", "perishable items", "perishable" } },
  },
}
