-- MonetaryStrategy: extracts dollar amounts preceded by label phrases.
-- Required fields: version, category, strategy, entries
-- Each entry requires: tag (string), labels (non-empty list)
-- Optional per-entry fields: bridges (words between label and amount), frequency (suffixes after amount)

return {
  version  = 1,
  category = "cost",
  strategy = "monetary",
  entries = {
    { tag = "MaintenanceCost",
      -- Label phrases that introduce the amount (trailing space is significant)
      labels    = { "maintenance fee ", "maintenance cost ", "maintenance " },
      -- Optional filler words between label and "$NNN"
      bridges   = { "is ", "of ", "approximately ", "around " },
      -- Optional frequency suffixes after the amount
      frequency = { "/month", "/mo", " per month", " monthly", " annually", "/year" } },

    { tag = "ShippingCost",
      labels    = { "shipping cost ", "shipping fee ", "freight charge ", "shipping " },
      bridges   = { "is ", "of " },
      frequency = { " per unit", " each", " per item" } },
  },
}
