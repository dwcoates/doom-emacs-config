/**
 * THE ONE CODE BLOCK: every block of code the webapp draws, whether a
 * markdown fence, a fence inside a metaprompt tree, or a reply's quoted text,
 * is the `pre.md-code` drawn here, so the shape (and everything the
 * stylesheet hangs on it) cannot drift between them. A source scan
 * (test/code-block.test.ts) fails any other module that writes the shape
 * itself.
 *
 * EVERY CODE BLOCK CARRIES A COPY CONTROL (owner request, 2026-10-20): a small
 * icon at the block's top right that copies the block's code to the
 * clipboard. It is an `<ar-button>` (control.ts), so every click owner that
 * leaves a control's click to the control (`CLICK_THROUGH_SELECTOR`) leaves
 * this one alone too: a click on it never selects, expands or folds the
 * bubble it sits in. The block is drawn as markup, re-drawn on every streamed
 * delta, so the control's click is answered once for the whole document
 * (`installCodeCopy`) rather than per element. The icon holds no text, so the
 * block's own text (its `textContent`, a drag-select across it) is its code
 * alone.
 */
import { writeClipboardText } from "./copy.js";
import { escapeHtml, highlightCode } from "./highlight.js";
import { log } from "./log.js";

/** The class a code block's copy control wears. */
export const CODE_COPY_CLASS = "md-code-copy";

/** The attribute a copy control wears for a moment after it copied. */
export const CODE_COPIED_ATTRIBUTE = "data-copied";

/** How long a copy control shows that it copied, in milliseconds. */
export const CODE_COPIED_MS = 1500;

/** What the copy control is called, on hover and to assistive technology. */
const COPY_LABEL = "Copy code";

/** Two overlapping sheets: the copy icon. */
const COPY_ICON =
  `<svg class="md-code-copy-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<rect x="5.5" y="5.5" width="8" height="8" rx="1.5"/>` +
  `<path d="M3.5 10.5h-0.5a1 1 0 0 1-1-1v-6.5a1 1 0 0 1 1-1h6.5a1 1 0 0 1 1 1v0.5"/>` +
  `</svg>`;

/** A check mark: what the control shows once it copied. */
const COPIED_ICON =
  `<svg class="md-code-copied-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<path d="M3 8.5l3.25 3.25l6.75-7"/>` +
  `</svg>`;

/** The copy control every code block carries. */
const COPY_CONTROL_HTML =
  `<ar-button class="${CODE_COPY_CLASS}" title="${COPY_LABEL}" aria-label="${COPY_LABEL}">` +
  `${COPY_ICON}${COPIED_ICON}</ar-button>`;

/**
 * BODY as the one code block: escaped, highlighted when LANG is a known
 * language (hljs escapes its own output), inside `pre.md-code`, with the copy
 * control ahead of the code.
 */
export function codeBlockHtml(body: string, lang: string): string {
  const html = highlightCode(body, lang);
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  return `<pre class="md-code">${COPY_CONTROL_HTML}<code class="hljs${langClass}">${html}</code></pre>`;
}

/**
 * The code CONTROL copies: the text of the code element of the block it sits
 * in, and the block's language ("" for none). A copy control is only ever
 * drawn inside a code block ahead of its code (`codeBlockHtml`), so a control
 * outside one is a bug, and throws.
 */
export function codeOfCopyControl(control: Element): { text: string; lang: string } {
  const code = control.closest("pre.md-code")?.querySelector(":scope > code");
  if (code === null || code === undefined) {
    throw new Error("a code block's copy control sits outside a code block's pre.md-code > code");
  }
  const langClass = [...code.classList].find((name) => name.startsWith("lang-"));
  return { text: code.textContent ?? "", lang: langClass?.slice("lang-".length) ?? "" };
}

/**
 * Answer a click on any code block's copy control in DOC: its block's code
 * goes to the clipboard through WRITE (`writeClipboardText` by default), and
 * the control wears `CODE_COPIED_ATTRIBUTE` for `CODE_COPIED_MS`. Answers the
 * uninstall.
 */
export function installCodeCopy(
  doc: Document,
  write: (text: string) => boolean = (text) => writeClipboardText(doc, text),
): () => void {
  const timers = new Map<Element, ReturnType<typeof setTimeout>>();
  const onClick = (event: MouseEvent): void => {
    if (!(event.target instanceof Element)) return;
    const control = event.target.closest(`.${CODE_COPY_CLASS}`);
    if (control === null) return;
    const { text, lang } = codeOfCopyControl(control);
    if (!write(text)) {
      log.error("a code block's copy control could not write its code to the clipboard", {
        operation: "code-block.copy",
        context: { characters: text.length, lang, outcome: "refused" },
      });
      return;
    }
    log.info("a code block's copy control copied its code", {
      operation: "code-block.copy",
      context: { characters: text.length, lang, outcome: "copied" },
    });
    clearTimeout(timers.get(control));
    control.setAttribute(CODE_COPIED_ATTRIBUTE, "");
    timers.set(
      control,
      setTimeout(() => {
        control.removeAttribute(CODE_COPIED_ATTRIBUTE);
        timers.delete(control);
      }, CODE_COPIED_MS),
    );
  };
  doc.addEventListener("click", onClick);
  return () => {
    doc.removeEventListener("click", onClick);
    for (const timer of timers.values()) clearTimeout(timer);
    timers.clear();
  };
}
