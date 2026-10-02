/**
 * THE READER'S TEXT SELECTION, READ IN ONE PLACE.
 *
 * Several click owners must tell a click apart from the click that ENDS a
 * drag-select (the bubble toggle, the bubble selection), and the copy
 * fallback must read what the reader highlighted. They all ask the same
 * question — what text is selected right now — so they ask it here, and a
 * source scan (test/selection.test.ts) fails any module that reads a
 * selection's text through `getSelection()` itself.
 */

/** Anything that owns a selection: a `Window` or a `Document`. */
export type SelectionSource = Pick<Window | Document, "getSelection">;

/**
 * The text SOURCE's selection currently holds, or "" when nothing is selected.
 *
 * A source with no selection object at all (`getSelection()` answering null:
 * a document with no browsing context) holds no selected text, and a
 * collapsed selection stringifies to "".
 */
export function selectedText(source: SelectionSource = window): string {
  return source.getSelection()?.toString() ?? "";
}
