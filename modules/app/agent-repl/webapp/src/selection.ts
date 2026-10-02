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
import { log } from "./log.js";

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

/** Where a selection starts and ends, compared by boundary, never by text. */
interface Extent {
  anchorNode: Node | null;
  anchorOffset: number;
  focusNode: Node | null;
  focusOffset: number;
}

/** SELECTION's extent, or null when it holds no text. */
function extentOf(selection: Selection | null): Extent | null {
  if (selection === null || selection.toString() === "") return null;
  const { anchorNode, anchorOffset, focusNode, focusOffset } = selection;
  return { anchorNode, anchorOffset, focusNode, focusOffset };
}

/** Whether A and B are the same extent (two absent extents are the same). */
function sameExtent(a: Extent | null, b: Extent | null): boolean {
  if (a === null || b === null) return a === b;
  return (
    a.anchorNode === b.anchorNode &&
    a.anchorOffset === b.anchorOffset &&
    a.focusNode === b.focusNode &&
    a.focusOffset === b.focusOffset
  );
}

/**
 * A CLICK THAT ENDS A DRAG-SELECT IS NOT A CLICK (owner ruling, 2026-10-02:
 * all text everywhere is selectable, and selecting it must not break the
 * click targets it lies on).
 *
 * The browser fires `click` when the button comes up, whether or not the
 * press-drag-release in between highlighted text, so a reader who drags across
 * a workspace row, a footer chip, a fold glyph or a link would otherwise also
 * activate it, and usually lose the highlight to whatever the click redraws.
 * This guard sits in front of every click handler on the page (document,
 * capture phase) and swallows exactly the mouse clicks whose gesture CHANGED
 * the selection into one holding text. Everything else is an ordinary click:
 *
 *   - nothing selected when the button comes up;
 *   - a selection that was already standing when the button went down and is
 *     unchanged (a click on a button beside an old highlight);
 *   - a click with no pointer behind it (`detail` 0: Enter or Space on a
 *     focused control, or a synthetic `element.click()`).
 *
 * Answers the uninstall.
 */
export function installSelectionClickGuard(doc: Document): () => void {
  let atPress: Extent | null = null;
  const onPointerDown = (): void => {
    atPress = extentOf(doc.getSelection());
  };
  const onClick = (event: MouseEvent): void => {
    const pressed = atPress;
    atPress = null;
    if (event.detail === 0) return;
    const now = extentOf(doc.getSelection());
    if (now === null || sameExtent(now, pressed)) return;
    event.stopPropagation();
    event.preventDefault();
    const target = event.target instanceof Element ? event.target : null;
    log.debug("a click that ended a drag-select activates nothing", {
      operation: "selection.click-swallowed",
      context: {
        target: target === null ? "none" : target.tagName.toLowerCase(),
        characters: selectedText(doc).length,
      },
    });
  };
  doc.addEventListener("pointerdown", onPointerDown, { capture: true });
  doc.addEventListener("click", onClick, { capture: true });
  return () => {
    doc.removeEventListener("pointerdown", onPointerDown, { capture: true });
    doc.removeEventListener("click", onClick, { capture: true });
  };
}
