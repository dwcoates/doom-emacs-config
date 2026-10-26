/**
 * merge-glyph — THE ONE CHARACTER every merge mark in the webapp draws: the
 * feed's merge bubble head, the footer's merges chip and its rows, and the
 * sidebar's merge queue heads. One author, so the surfaces never drift to
 * two different arrows for the same act.
 */

/** The merge glyph. */
export const MERGE_GLYPH = "⇄";

/**
 * The character each feed merge glyph NAME draws as
 * (`frontend.v1.FeedMergeGlyph.icon`, from the shared vocabulary's
 * `feed_merge_head_glyph`).
 */
export const MERGE_GLYPH_NAMES: Readonly<Record<string, string>> = Object.freeze({ merge: MERGE_GLYPH });
