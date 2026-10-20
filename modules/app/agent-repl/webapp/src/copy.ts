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
 * WebKit view embedded in an application that owns the keymap. On the NS port
 * the xwidget's `keyDown:` forwards a plain key to Emacs unless a page input
 * has focus, so `y` over highlighted page text is Emacs's to handle:
 * `agent-repl-frontend-copy-selection` (lisp/frontend.el) asks this page for
 * its selection and writes it with `kill-new`. A copy the browser itself
 * performs is the other path, and this module backs it.
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
 * It adds no visible element. The one copy control in this app, a code block's
 * (code-block.ts), copies its block's code through `writeClipboardText` below,
 * never through the selection.
 *
 * THE COPY CHORDS. `C-c` and Cmd-c never reach that `keyDown:`: a key with
 * Control or Command held is a key equivalent, which WKWebView's
 * `performKeyEquivalent:` takes first and hands to the page as a DOM
 * `keydown` (the reason src/nsxwidget.m already listens for `C-g` in the page
 * rather than in `keyDown:`). So the page answers those two chords itself:
 * over a nonempty selection it runs the browser's own copy command, whose
 * `copy` event the fallback above answers with the selection's text. Every
 * other chord, and either chord with nothing selected, is left untouched.
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

/** Whether EVENT is `C-c` or Cmd-c: the C key under exactly one of Control and Command. */
export function isCopyChord(event: KeyboardEvent): boolean {
  return (
    event.key.toLowerCase() === "c" &&
    event.ctrlKey !== event.metaKey &&
    !event.altKey &&
    !event.shiftKey
  );
}

/**
 * Install the `C-c` / Cmd-c copy chords on DOC, and answer a teardown that
 * removes them. COPY runs the browser's copy command and answers whether the
 * browser carried it out; the default is `document.execCommand("copy")`, which
 * fires the `copy` event `installCopyFallback` answers.
 */
export function installCopyChords(
  doc: Document,
  copy: () => boolean = () => doc.execCommand("copy"),
): () => void {
  const onKeyDown = (event: KeyboardEvent): void => {
    if (!isCopyChord(event)) return;
    const characters = selectedText(doc).length;
    if (characters === 0) return;

    const chord = event.metaKey ? "Cmd-c" : "C-c";
    if (!copy()) {
      log.error("the browser refused to copy the selection", {
        operation: "copy.chord",
        context: { chord, characters, outcome: "refused" },
      });
      return;
    }
    event.preventDefault();
    log.debug("a copy chord copied the selection", {
      operation: "copy.chord",
      context: { chord, characters, outcome: "copied" },
    });
  };

  doc.addEventListener("keydown", onKeyDown);
  return () => doc.removeEventListener("keydown", onKeyDown);
}

/**
 * Write TEXT to the clipboard through the browser's copy command, and answer
 * whether it was written. COPY runs the command (`document.execCommand("copy")`
 * by default), which must run inside a user gesture; the `copy` event it fires
 * is answered here, in the capture phase and ahead of `installCopyFallback`,
 * with TEXT instead of the reader's selection. A copy whose event never came,
 * or came with no writable `clipboardData`, wrote nothing.
 */
export function writeClipboardText(
  doc: Document,
  text: string,
  copy: () => boolean = () => doc.execCommand("copy"),
): boolean {
  let written = false;
  const onCopy = (event: Event): void => {
    const clipboardData = (event as ClipboardEvent).clipboardData;
    if (clipboardData === null || clipboardData === undefined) return;
    clipboardData.setData("text/plain", text);
    event.preventDefault();
    event.stopImmediatePropagation();
    written = true;
  };
  doc.addEventListener("copy", onCopy, true);
  try {
    copy();
  } finally {
    doc.removeEventListener("copy", onCopy, true);
  }
  return written;
}
