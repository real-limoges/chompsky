-- Triage configuration: word lists, thresholds, and conflict rules.
-- All domain-specific triage data lives here, not in Haskell code.

return {
    -- Phrases that signal an ambiguous condition without stating it explicitly.
    -- Each entry: { category = "...", phrases = { "...", ... } }
    ambiguous_phrases = {
        {
            category = "condition",
            phrases = {
                "opportunity",
                "handyman",
                "fixer",
                "tlc",
                "as-is",
                "as is",
                "cosmetic",
                "needs work",
                "needs updating",
                "needs some tlc",
                "deferred maintenance",
                "sweat equity",
                "investor special",
                "potential",
                "bring your vision",
                "make it your own",
                "diamond in the rough",
                "priced to sell",
                "priced reflecting condition",
                "priced to reflect",
                "sold as-is where-is",
            },
        },
    },

    -- Vocabulary watch lists: flag when many words match but parser finds nothing.
    -- Each entry: { category = "...", words = { "...", ... } }
    vocab_watch_lists = {
        {
            category = "renovation",
            words = {
                "roof", "hvac", "kitchen", "bath", "bathroom",
                "window", "windows", "floor", "floors", "flooring",
                "plumbing", "electrical", "paint", "painted",
                "appliance", "appliances", "water", "heater",
                "siding", "deck", "landscaping", "driveway",
                "renovated", "renovation", "remodel", "remodeled",
                "updated", "update", "replaced", "new",
                "installed", "upgraded", "refinished",
                "restored", "rebuilt", "repaired", "refurbished",
            },
        },
    },

    -- Conflicting tag pairs: flag when both positive and negative tags appear.
    -- Each entry: { category = "...", positive = { ... }, negative = { ... } }
    conflicting_tag_pairs = {
        {
            category = "condition",
            positive = { "Excellent", "Good" },
            negative = { "Fair", "Poor" },
        },
    },

    -- Word count above which a zero-feature remark is flagged.
    long_no_signal_threshold = 40,

    -- Vocab word count above which a zero-extraction remark is flagged.
    vocab_threshold = 3,

    -- Non-ASCII character ratio above which text is flagged as unrecognizable.
    unrecognizable_ratio_threshold = 0.3,
}
