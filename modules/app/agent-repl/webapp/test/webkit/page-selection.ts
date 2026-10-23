/**
 * THE SELECTION A WEBKIT PAGE HANDS BACK. Shared by every webkit page that
 * reports what a gesture selected, so each reads it, and clears it for the
 * next gesture, the same way.
 */

/** DOC's selected text, then clear the selection. */
export function takePageSelection(doc: Document): string {
  const selection = doc.getSelection();
  const text = selection === null ? "" : selection.toString();
  selection?.removeAllRanges();
  return text;
}
