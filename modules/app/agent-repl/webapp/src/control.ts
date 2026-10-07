/**
 * THE ONE CONTROL: every clickable control in the webapp is built here, and
 * none of them is a `<button>`.
 *
 * WHY NOT A BUTTON (owner ruling, 2026-10-02: "all text must be supported,
 * everywhere in the webapp"). WebKit never starts a text selection inside a
 * `<button>`: a drag that begins on a button's label selects nothing, whatever
 * `user-select` says. So a control is an `<ar-button>` element instead, an
 * undefined custom element that WebKit treats as ordinary text, and this module
 * gives it back everything the button carried:
 *
 *   - the `button` role, for accessibility;
 *   - focusability (`tabindex` 0), dropped while disabled, as a disabled
 *     button drops it;
 *   - keyboard activation: Enter clicks on keydown and Space on keyup, as a
 *     button does;
 *   - a `disabled` property, reflected as `aria-disabled="true"`, and a refusal
 *     of every click while it is set, so a disabled control activates nothing.
 *
 * The tag is a TYPE selector, like `button`, so a stylesheet rule that named
 * `button` names `ar-button` at exactly the same specificity, and the base look
 * the user agent gave a button is restated once in styles.css under
 * `:where(ar-button)`, which every author rule outranks. A source scan
 * (test/control.test.ts) fails any module that builds a `<button>` itself.
 */
import { log } from "./log.js";

/** The control's tag: a type selector, as `button` was. */
export const CONTROL_TAG = "ar-button";

/** Every control on the page, for a query or a `closest`. */
export const CONTROL_SELECTOR = CONTROL_TAG;

/** A control: the element, plus the `disabled` state a button carried. */
export type Control = HTMLElement & { disabled: boolean };

/** Whether NODE is a control this module built. */
export function isControl(node: unknown): node is Control {
  return node instanceof HTMLElement && node.localName === CONTROL_TAG;
}

/** Whether CONTROL is disabled, read off its `aria-disabled` attribute. */
function disabledOf(control: HTMLElement): boolean {
  return control.getAttribute("aria-disabled") === "true";
}

/** Set CONTROL's disabled state: `aria-disabled`, and focusability with it. */
function setDisabled(control: HTMLElement, disabled: boolean): void {
  if (disabled) {
    control.setAttribute("aria-disabled", "true");
    control.tabIndex = -1;
  } else {
    control.removeAttribute("aria-disabled");
    control.tabIndex = 0;
  }
}

/**
 * Give EL what a button carries: the `button` role, focusability, Enter
 * (keydown) and Space (keyup) activation, and the refusal of every click while
 * `aria-disabled="true"` stands. `createControl` arms every control through
 * here, and so does every other element that acts as a button (a footer chip,
 * a bubble's head, an editor link), so none of them hand-rolls its keys.
 *
 * Keys pressed while focus sits on something INSIDE EL belong to that thing,
 * so only a key whose target is EL itself activates it.
 */
export function armButtonRole(el: HTMLElement): void {
  el.setAttribute("role", "button");
  el.tabIndex = disabledOf(el) ? -1 : 0;

  // REFUSAL FIRST: registered before any caller's listener, in the capture
  // phase, so a disabled element's click reaches no handler on it or above it.
  el.addEventListener(
    "click",
    (event) => {
      if (!disabledOf(el)) return;
      event.stopImmediatePropagation();
      event.preventDefault();
      log.debug("a click on a disabled control activates nothing", {
        operation: "control.click-refused",
        context: { classes: el.className },
      });
    },
    { capture: true },
  );

  // KEYBOARD ACTIVATION, as a button: Enter on keydown, Space on keyup (its
  // keydown only stops the page from scrolling). The click is synthetic, so
  // the refusal above still decides whether anything runs.
  el.addEventListener("keydown", (event) => {
    if (event.target !== el) return;
    if (event.key === "Enter") {
      event.preventDefault();
      el.click();
    } else if (event.key === " ") {
      event.preventDefault();
    }
  });
  el.addEventListener("keyup", (event) => {
    if (event.target !== el || event.key !== " ") return;
    event.preventDefault();
    el.click();
  });
}

/**
 * Build one control in DOC: an armed `<ar-button>` with a `disabled`
 * property. The caller gives it its class, label and click handler exactly as
 * it gave a button.
 */
export function createControl(doc: Document = document): Control {
  const el = doc.createElement(CONTROL_TAG);
  armButtonRole(el);
  Object.defineProperty(el, "disabled", {
    configurable: false,
    enumerable: true,
    get: () => disabledOf(el),
    set: (value: boolean) => setDisabled(el, value),
  });
  return el as Control;
}

/** What a labelled control is built from (`labelledControl`). */
export interface LabelledControlSpec {
  /** The control's whole class list. */
  className: string;
  /** The attribute it is found by, and that attribute's value. */
  hook: readonly [attribute: string, value: string];
  /** The words it shows. */
  label: string;
  /** What a click does; the click's default is prevented first. */
  onClick: (control: Control) => void;
}

/**
 * A control that says LABEL, wears its class, carries its hook and runs its
 * click: the shape every row control shares, built once so no two rows hand
 * roll it differently.
 */
export function labelledControl(spec: LabelledControlSpec): Control {
  const control = createControl();
  control.className = spec.className;
  control.setAttribute(spec.hook[0], spec.hook[1]);
  control.textContent = spec.label;
  control.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    spec.onClick(control);
  });
  return control;
}
