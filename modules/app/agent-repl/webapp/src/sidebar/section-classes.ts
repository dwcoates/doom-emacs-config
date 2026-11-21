/**
 * The classes the rail's four sections are drawn with.
 *
 * A LEAF MODULE, importing nothing, because modules evaluate these names at
 * their top level (section-fold.ts's `FOCUS_FOLDED_SECTIONS`) and the sidebar's
 * modules import one another in cycles: kept in band-fit.ts, they were still
 * undefined when section-fold.ts evaluated inside the cycle
 * band-fit -> row -> children-fold -> section-fold -> band-fit, whenever
 * band-fit.ts was imported first. A module that imports nothing is never in a
 * cycle, so its names are always defined when anything reads them.
 */
export const OPEN_SECTION_CLASS = "open-section";
export const HIDDEN_SECTION_CLASS = "hidden-section";
export const MERGED_SECTION_CLASS = "merged-section";
export const MERGE_QUEUE_SECTION_CLASS = "merge-queue-section";
