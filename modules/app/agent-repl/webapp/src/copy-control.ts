/**
 * THE ONE COPY CONTROL: the small icon that copies a piece of the feed to the
 * clipboard (owner requests, 2026-10-20). Every kind of it (`CopyKind`) is the
 * same `<ar-button>` with the same two icons, the same check-mark moment after
 * it copied, and the same click handler; a kind decides only WHAT it copies,
 * through the resolver `installCopyControls` is handed for it.
 *
 * It is an `<ar-button>` (control.ts), so every click owner that leaves a
 * control's click to the control (`CLICK_THROUGH_SELECTOR`) leaves this one
 * alone too: a click on it never selects, expands or folds the bubble it sits
 * in. Controls are drawn as markup inside content redrawn on every streamed
 * delta, so their click is answered once for the whole document rather than
 * per element. The icons hold no text, so the text around a control (its
 * `textContent`, a drag-select across it) is unchanged by it.
 */
import { writeClipboardText } from "./copy.js";
import { log } from "./log.js";
import { unreachableArm } from "./rpc/strict.js";

/** The class every copy control wears. */
export const COPY_CONTROL_CLASS = "copy-control";

/** The attribute naming what a copy control copies: its `CopyKind`. */
export const COPY_KIND_ATTRIBUTE = "data-copies";

/** What a copy control copies: a code block's code, a whole prompt, or a whole final response. */
export const COPY_KINDS = ["code", "prompt", "response"] as const;
export type CopyKind = (typeof COPY_KINDS)[number];

/** The attribute a copy control wears for a moment after it copied. */
export const COPIED_ATTRIBUTE = "data-copied";

/** How long a copy control shows that it copied, in milliseconds. */
export const COPIED_MS = 1500;

/** Two overlapping sheets: the copy icon. */
const COPY_ICON =
  `<svg class="copy-control-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<rect x="5.5" y="5.5" width="8" height="8" rx="1.5"/>` +
  `<path d="M3.5 10.5h-0.5a1 1 0 0 1-1-1v-6.5a1 1 0 0 1 1-1h6.5a1 1 0 0 1 1 1v0.5"/>` +
  `</svg>`;

/** A check mark: what the control shows once it copied. */
const COPIED_ICON =
  `<svg class="copy-control-copied-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<path d="M3 8.5l3.25 3.25l6.75-7"/>` +
  `</svg>`;

/**
 * The copy control of KIND as markup: wearing `COPY_CONTROL_CLASS` and
 * CLASSNAME (where its kind places it), and LABEL on hover and for assistive
 * technology. LABEL and CLASSNAME are the caller's literals, never text read
 * off the wire.
 */
export function copyControlHtml(kind: CopyKind, className: string, label: string): string {
  return (
    `<ar-button class="${COPY_CONTROL_CLASS} ${className}" ${COPY_KIND_ATTRIBUTE}="${kind}" ` +
    `title="${label}" aria-label="${label}">${COPY_ICON}${COPIED_ICON}</ar-button>`
  );
}

/** The text each control built by `boundCopyControl` copies. */
const boundText = new WeakMap<Element, string>();

/**
 * The copy control of KIND as an element that copies TEXT, for a kind whose
 * text is not read off the page around the control (a prompt's, which is the
 * person's markdown rather than what it renders as). Its resolver is
 * `boundCopyText`.
 */
export function boundCopyControl(kind: CopyKind, className: string, label: string, text: string): HTMLElement {
  const template = document.createElement("template");
  template.innerHTML = copyControlHtml(kind, className, label);
  const control = template.content.firstElementChild;
  if (!(control instanceof HTMLElement)) throw new Error("copyControlHtml drew no element");
  boundText.set(control, text);
  return control;
}

/**
 * The text a control built by `boundCopyControl` copies. A control of a bound
 * kind with no text bound to it is a bug, and throws.
 */
export function boundCopyText(control: Element): CopiedText {
  const text = boundText.get(control);
  if (text === undefined) {
    throw new Error(`a ${control.getAttribute(COPY_KIND_ATTRIBUTE)} copy control has no text bound to it`);
  }
  return { text, context: {} };
}

/** What a copy control copies, and what its log record names it by. */
export interface CopiedText {
  text: string;
  context: Record<string, string | number>;
}

/** Each kind's answer to which text a control of it copies. */
export type CopyResolvers = Record<CopyKind, (control: Element) => CopiedText>;

/** Whether VALUE names a kind of copy control. */
function isCopyKind(value: string | null): value is CopyKind {
  return (COPY_KINDS as readonly (string | null)[]).includes(value);
}

/**
 * Answer a click on any copy control in DOC: the text RESOLVERS name for its
 * kind goes to the clipboard through WRITE (`writeClipboardText` by default),
 * and the control wears `COPIED_ATTRIBUTE` for `COPIED_MS`. A control with no
 * kind is a bug, and throws. Answers the uninstall.
 */
export function installCopyControls(
  doc: Document,
  resolvers: CopyResolvers,
  write: (text: string) => boolean = (text) => writeClipboardText(doc, text),
): () => void {
  const timers = new Map<Element, ReturnType<typeof setTimeout>>();
  const onClick = (event: MouseEvent): void => {
    if (!(event.target instanceof Element)) return;
    const control = event.target.closest(`.${COPY_CONTROL_CLASS}`);
    if (control === null) return;
    const kind = control.getAttribute(COPY_KIND_ATTRIBUTE);
    if (!isCopyKind(kind)) return unreachableArm("CopyKind", String(kind));
    const { text, context } = resolvers[kind](control);
    if (!write(text)) {
      log.error("a copy control could not write its text to the clipboard", {
        operation: "copy-control.copy",
        context: { kind, characters: text.length, ...context, outcome: "refused" },
      });
      return;
    }
    log.info("a copy control copied its text", {
      operation: "copy-control.copy",
      context: { kind, characters: text.length, ...context, outcome: "copied" },
    });
    clearTimeout(timers.get(control));
    control.setAttribute(COPIED_ATTRIBUTE, "");
    timers.set(
      control,
      setTimeout(() => {
        control.removeAttribute(COPIED_ATTRIBUTE);
        timers.delete(control);
      }, COPIED_MS),
    );
  };
  doc.addEventListener("click", onClick);
  return () => {
    doc.removeEventListener("click", onClick);
    for (const timer of timers.values()) clearTimeout(timer);
    timers.clear();
  };
}
