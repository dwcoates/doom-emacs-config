// The sidebar's ONE name popup: a text box and a button under the control
// that opened it, which asks for a name and sends it. A subsection's create
// and rename, and a workspace's rename, all open it, so every name the
// sidebar asks for is asked the same way.

import { createControl } from "../control.js";
import { windowOf } from "../dom.js";
import { log } from "../log.js";
import type { SidebarContext } from "./context.js";

/** The class of the popup that asks for a name. */
export const NAME_POPUP_CLASS = "sb-subsection-popup";

/** What a name popup asks for and how it sends it. */
export interface NamePopupSpec {
  /**
   * What is being named, as one word: it names the popup's data attributes
   * (`data-<subject>-popup`, `-name`, `-submit`) and its log records
   * (`sidebar.<subject>.name`, `sidebar.<subject>.popup-open`,
   * `sidebar.<subject>.popup-close`).
   */
  subject: string;
  placeholder: string;
  value: string;
  submitLabel: string;
  /**
   * Send NAME, with CONTROL the element a refusal is drawn beside. Answers
   * whether the daemon took it; a refusal keeps the popup open.
   */
  send: (name: string, control: HTMLElement) => Promise<boolean>;
}

/**
 * Open the name popup under ANCHOR, replacing any open one.
 *
 * Enter or the button sends; Escape or a press anywhere outside it closes it.
 * It is one of the rail's dropdowns (`dropdowns.ts`), so opening another
 * dropdown closes it, and so does the rail losing the user (`transients.ts`:
 * the page losing focus, as a click into Emacs outside the page does, which
 * the page never sees as a press). A blank name is not sent: the contract
 * refuses one, so the popup keeps waiting rather than provoking the refusal.
 */
export function openNamePopup(sc: SidebarContext, anchor: HTMLElement, spec: NamePopupSpec): HTMLElement {
  const subject = spec.subject;
  const popup = document.createElement("div");
  popup.className = NAME_POPUP_CLASS;
  popup.setAttribute(`data-${subject}-popup`, "");
  const box = anchor.getBoundingClientRect();
  popup.style.top = `${box.bottom + 4}px`;
  popup.style.left = `${Math.max(4, box.right - 220)}px`;

  const input = document.createElement("input");
  input.type = "text";
  input.className = "sb-subsection-name";
  input.setAttribute(`data-${subject}-name`, "");
  input.placeholder = spec.placeholder;
  input.value = spec.value;
  const submit = createControl();
  submit.className = "sb-form-go";
  submit.setAttribute(`data-${subject}-submit`, "");
  submit.textContent = spec.submitLabel;
  popup.append(input, submit);

  const win = windowOf(anchor);
  const outside = (event: PointerEvent): void => {
    if (!popup.contains(event.target as Node)) close("a press outside it");
  };
  const close = (why: string): void => {
    log.debug(`closing the ${subject} name popup`, { operation: `sidebar.${subject}.popup-close`, context: { why } });
    popup.remove();
    win.document.removeEventListener("pointerdown", outside, true);
    sc.dropdowns.released(popup);
  };
  const send = (): void => {
    const name = input.value.trim();
    if (name === "") return;
    log.info(`naming a sidebar ${subject}`, { operation: `sidebar.${subject}.name`, context: { length: name.length } });
    void spec.send(name, submit).then((ok) => {
      if (ok) close("the name was sent");
    });
  };
  submit.addEventListener("click", (event) => {
    event.preventDefault();
    send();
  });
  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") send();
    if (event.key === "Escape") close("Escape");
  });
  win.document.addEventListener("pointerdown", outside, true);
  // Registered before it is drawn, so the popup it replaces is closed (and
  // stops listening) first.
  sc.dropdowns.opened({ kind: "subsection-popup", element: popup, openers: [anchor], close });
  log.debug(`opening the ${subject} name popup`, {
    operation: `sidebar.${subject}.popup-open`,
    context: { submit: spec.submitLabel },
  });
  sc.arrange.layer.appendChild(popup);
  input.focus();
  return popup;
}
