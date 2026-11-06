-- Triage configuration: word lists, thresholds, and conflict rules.
-- All domain-specific triage data lives here, not in Haskell code.
-- Categories named here should be ones the parsers in parsers/ emit.

return {
    -- Phrases that make a remark ambiguous for a category: the text talks about
    -- the category without saying anything a parser can pin down.
    -- Each entry: { category = "...", phrases = { "...", ... } }
    ambiguous_phrases = {
        {
            category = "category",
            phrases = {
                "miscellaneous",
                "misc",
                "assorted",
                "mixed lot",
                "various items",
                "odds and ends",
                "sundries",
                "general merchandise",
            },
        },
        {
            category = "cost",
            phrases = {
                "price on request",
                "call for pricing",
                "to be determined",
                "tbd",
                "negotiable",
            },
        },
    },

    -- Vocabulary watch lists: flag when many of a category's words appear but
    -- its parser extracts nothing, which usually means the parser is missing a
    -- phrase.
    -- Each entry: { category = "...", words = { "...", ... } }
    vocab_watch_lists = {
        {
            category = "category",
            words = {
                "laptop", "tablet", "phone", "monitor", "television", "printer",
                "sofa", "couch", "chair", "desk", "table", "shelving", "cabinet",
                "shirt", "jacket", "shoes", "uniform",
                "produce", "dairy", "meat", "frozen", "refrigerated",
            },
        },
        {
            category = "activity",
            words = {
                "inspection", "audit", "count", "recount", "stocktake",
                "walkthrough", "repair", "calibration", "servicing",
            },
        },
    },

    -- Conflicting tag pairs: flag when a category holds a positive and a
    -- negative tag at once. The shipped parsers emit no mutually exclusive
    -- tags, so none are declared.
    -- Each entry: { category = "...", positive = { ... }, negative = { ... } }
    conflicting_tag_pairs = {},

    -- Word count above which a zero-feature remark is flagged.
    long_no_signal_threshold = 40,

    -- Vocab word count above which a zero-extraction remark is flagged.
    vocab_threshold = 3,

    -- Non-ASCII character ratio above which text is flagged as unrecognizable.
    unrecognizable_ratio_threshold = 0.3,
}
