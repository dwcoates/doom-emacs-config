/**
 * name-glyph — the glyph the daemon puts before a workspace's name
 * (`frontend.v1.RosterRowNameGlyph`, `frontend.v1.TopbarWorkspaceNameGlyph`):
 * the thumbs up an emoji-ack workspace wears. THE ONE DRAWING every surface
 * that draws a workspace's name uses, so the sidebar's rows and the topbar
 * draw the glyph alike.
 *
 * The glyph is its own element BEFORE the name's, never part of the name's
 * text: the name stays the slug the rename, the search and the name's ink
 * read, and the name's tint and breath never touch the glyph. It is sized
 * by the stylesheet's `.name-glyph` at the name's own font size, so the emoji
 * is never drawn larger than the letters beside it.
 */

/** The class every drawn name glyph carries, which the stylesheet sizes. */
export const NAME_GLYPH_CLASS = "name-glyph";

/** A name glyph from either surface's message: its text, verbatim. */
export interface NameGlyph {
  text: string;
}

/** Draw GLYPH, the daemon's glyph for the name at PATH, verbatim. */
export function drawNameGlyph(glyph: NameGlyph, path: string): HTMLElement {
  if (glyph.text === "") throw new Error(`${path}: a name glyph carries its glyph`);
  const element = document.createElement("span");
  element.className = NAME_GLYPH_CLASS;
  element.textContent = glyph.text;
  return element;
}
