/**
 * Copying the highlighted text out of the feed.
 *
 * The webapp runs inside Emacs' WKWebView, which has no menu bar and no
 * Cmd-C plumbing of its own, so a mouse-highlighted range is otherwise
 * unreachable: the browser's native copy gesture never fires. Two chords
 * copy the selection instead — `C-c` (the terminal reflex) and a bare `y`
 * (the vim reflex) — matching how the surrounding Emacs frame is driven.
 *
 * A chord is only ours when the highlight is real and the key is not
 * already spoken for: inside the composer (or any editable field) `y` is a
 * letter and `C-c`/Cmd-C is the field's own copy, so both are left alone.
 *
 * `installCopyKeys` is the only DOM-facing piece; every decision it makes
 * lives in the pure helpers above it.
 */

/** The parts of a KeyboardEvent a copy chord is decided from. */
export interface KeyChord {
  key: string;
  ctrlKey: boolean;
  metaKey: boolean;
  altKey: boolean;
  shiftKey: boolean;
}

/** The parts of an event target that decide whether it owns its own keys. */
export interface TargetShape {
  tagName: string;
  isContentEditable: boolean;
}

/** Elements that type their own text, so `y` is a letter and copy is theirs. */
const EDITABLE_TAGS = ["INPUT", "TEXTAREA", "SELECT"];

/** True when the chord is a copy gesture: `C-c`, Cmd-C, or a bare `y`. */
export function isCopyChord(e: KeyChord): boolean {
  if (e.altKey || e.shiftKey) return false;
  const key = e.key.toLowerCase();
  if (key === "c") return e.ctrlKey || e.metaKey;
  if (key === "y") return !e.ctrlKey && !e.metaKey;
  return false;
}

/** True when the target types its own text and must keep its keys. */
export function ownsItsKeys(target: TargetShape | null): boolean {
  if (!target) return false;
  return target.isContentEditable || EDITABLE_TAGS.includes(target.tagName.toUpperCase());
}

/**
 * The whole copy decision: null leaves the event to the browser, a string
 * is the text to place on the clipboard. A collapsed or whitespace-only
 * highlight is nothing to copy, so the chord stays inert.
 */
export function copyAction(opts: {
  event: KeyChord;
  target: TargetShape | null;
  selection: string;
}): string | null {
  if (!isCopyChord(opts.event)) return null;
  if (ownsItsKeys(opts.target)) return null;
  if (opts.selection.trim() === "") return null;
  return opts.selection;
}

/** The clipboard mechanism the embedder grants (the async Clipboard API). */
export interface ClipboardWriter {
  writeText?: (text: string) => Promise<void>;
}

/**
 * Put TEXT on the clipboard, rejecting when the embedder grants no writer.
 * The webview is served from a loopback origin, which is a secure context,
 * so the Clipboard API is exposed and a key gesture is the user activation
 * it wants. Its absence is a broken embedding and is raised, never
 * swallowed into a copy that silently did nothing.
 */
export async function writeSelection(text: string, writer: ClipboardWriter): Promise<void> {
  if (!writer.writeText) {
    throw new Error("clipboard copy rejected: this embedding exposes no Clipboard API");
  }
  await writer.writeText(text);
}

/** The live document's clipboard mechanism. */
function domWriter(): ClipboardWriter {
  return {
    writeText: navigator.clipboard?.writeText
      ? (text: string) => navigator.clipboard.writeText(text)
      : undefined,
  };
}

/**
 * Arm `C-c` / `y` on ROOT: either copies the highlighted text when the
 * highlight is real and the key is not the target's own.
 * Copy failures are re-raised on the macrotask queue rather than
 * disappearing into an unhandled rejection.
 */
export function installCopyKeys(
  root: Document | HTMLElement,
  writer: ClipboardWriter = domWriter(),
  selection: () => string = () => window.getSelection()?.toString() ?? "",
): void {
  root.addEventListener("keydown", (event) => {
    const e = event as KeyboardEvent;
    const target = e.target instanceof HTMLElement ? e.target : null;
    const text = copyAction({
      event: e,
      target: target && { tagName: target.tagName, isContentEditable: target.isContentEditable },
      selection: selection(),
    });
    if (text === null) return;
    e.preventDefault();
    void writeSelection(text, writer).catch((err: unknown) => {
      setTimeout(() => {
        throw err;
      });
    });
  });
}
