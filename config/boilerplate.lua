-- Boilerplate phrases stripped from input during the clean stage.
-- min_position: phrases appearing before this character offset are NOT stripped
--   (protects content near the start of the text).
-- phrases: exact strings to remove (case-insensitive matching applied by pipeline).

return {
  min_position = 50,
  phrases = {
    "this information is provided as-is",
    "all data subject to verification",
    "details may vary",
  },
}
