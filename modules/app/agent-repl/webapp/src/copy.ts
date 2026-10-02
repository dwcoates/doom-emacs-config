/**
 * THE COPY FALLBACK.
 *
 * The owner's ruling (2026-09-14): "ensure that everything in the webapp (all
 * text) can be copied to clipboard; I need to be able to do this to easily
 * relay errors." Selection is settled in `styles.css` — nothing a reader can
 * see is `user-select: none` any more, outside the pure-control allowlist the
 * stylesheet test pins. This module settles the second half: that the SELECTED
 * text actually reaches the clipboard.
 *
 * WHY A FALLBACK AT ALL. The page's production host is an Emacs xwidget, a
 * WebKit view embedded in an application that owns the keymap. The page itself
 * never intercepts Cmd/Ctrl-C — no keydown handler in `src/` looks at the C
 * key — so the browser's own copy is the first path and stays the first path.
 * But the xwidget's native copy is not something this page can verify, and a
 * `copy` event that fires with an empty clipboard payload would silently hand
 * the reader nothing. So: when a `copy` event DOES reach the document and the
 * event carries a writable `clipboardData`, this writes the current selection's
 * own text onto it, which is exactly what the native path would have written.
 *
 * IT IS NOT A SECOND VOCABULARY. It copies the selection's own text (`selectedText`)
 * and nothing else — no reformatting, no synthesised text, no per-component
 * knowledge. A collapsed or absent selection is left entirely alone, so a copy
 * aimed at a focused input or textarea (whose selection is not the document's)
 * keeps the browser's behaviour untouched.
 *
 * It adds no visible element. There is no copy button anywhere in this app.
 */
import { log } from "./log.js";
import { selectedText } from "./selection.js";

/**
 * Install the document-level `copy` fallback, and answer a teardown that
 * removes it.
 */
export function installCopyFallback(doc: Document): () => void {
  const onCopy = (event: Event): void => {
    const clipboardData = (event as ClipboardEvent).clipboardData;
    if (clipboardData === null || clipboardData === undefined) return;

    const text = selectedText(doc);
    if (text === "") return;

    clipboardData.setData("text/plain", text);
    event.preventDefault();
    log.debug("the copy fallback wrote the selection to the clipboard", {
      operation: "copy.fallback",
      context: { characters: text.length },
    });
  };

  doc.addEventListener("copy", onCopy);
  return () => doc.removeEventListener("copy", onCopy);
}
