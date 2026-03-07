-- Abbreviation expansion applied during normalization.
-- words: single-token abbreviations mapped to their expanded form.
-- phrases: multi-word patterns replaced with a canonical form.

return {
  words = {
    qty   = "quantity",
    pcs   = "pieces",
    approx = "approximately",
    mfg   = "manufacturing",
    dept  = "department",
    inv   = "inventory",
    whse  = "warehouse",
    equip = "equipment",
    maint = "maintenance",
  },
  phrases = {
    { match = "temp controlled", replace = "temperature controlled" },
    { match = "clim ctrl",       replace = "climate controlled" },
  },
}
