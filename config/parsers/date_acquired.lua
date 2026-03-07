-- TriggerStrategy: extracts a 4-digit year that follows a trigger phrase.
-- Required fields: version, category, strategy, entries
-- Each entry requires: tag (string), triggers (non-empty list of strings)
-- Each trigger string should include any trailing space before the year.

return {
  version  = 1,
  category = "date_acquired",
  strategy = "trigger",
  entries = {
    { tag = "AcquisitionYear",
      triggers = { "acquired in ", "purchased in ", "received in ", "obtained in ", "procured " } },

    { tag = "DisposalYear",
      triggers = { "disposed in ", "sold in ", "retired in ", "decommissioned in " } },
  },
}
