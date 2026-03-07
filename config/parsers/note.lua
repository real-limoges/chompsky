-- CaptureStrategy: captures free text following a trigger phrase up to a terminator.
-- Required fields: version, category, strategy, entries
-- Each entry requires: tag (string), triggers (non-empty list), terminators (string of chars)
-- terminators is a string whose individual characters each act as a stop character.

return {
  version  = 1,
  category = "note",
  strategy = "capture",
  entries = {
    { tag = "HandlingNote",
      -- Stop capturing at any of these characters
      terminators = ".!\n;",
      triggers = {
        "special instructions",
        "handling instructions",
        "handle with care",
        "fragile",
        "do not stack",
        "keep upright",
        "temperature sensitive",
      } },

    { tag = "StorageNote",
      terminators = ".!\n;",
      triggers = {
        "storage requirements",
        "store in",
        "keep away from",
        "do not expose to",
        "climate controlled",
      } },
  },
}
