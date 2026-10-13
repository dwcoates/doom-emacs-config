/**
 * connect-hook — THE ONE ELEMENT THAT RUNS HOOKS AS IT JOINS THE DOCUMENT.
 *
 * A plain element has no "attached" moment a renderer can wait on: the row a
 * renderer builds is placed on the page later, by whoever lays the feed out.
 * A custom element does: `connectedCallback` runs synchronously inside the DOM
 * operation that attaches it (before any frame is painted, whether or not the
 * webview is visible), and again on every later re-attachment. So work that
 * must happen once the element is on the page (a measurement against its
 * containing block, a focus) is registered here as a hook rather than guessed
 * at with a timer.
 *
 * Each tag is defined on first use with a class of its own, since the
 * registry takes one constructor per tag.
 */

/** An element that runs its hooks every time it joins the document. */
export interface ConnectHookElement extends HTMLElement {
  onConnect(hook: () => void): void;
}

/** A fresh connect-hook class: the registry takes one constructor per tag. */
function connectHookClass(): CustomElementConstructor {
  return class extends HTMLElement implements ConnectHookElement {
    private readonly hooks: Array<() => void> = [];

    onConnect(hook: () => void): void {
      this.hooks.push(hook);
    }

    connectedCallback(): void {
      for (const hook of this.hooks) hook();
    }
  };
}

/** A fresh element of TAG, the tag defined as a connect-hook element on first use. */
export function createConnectHookElement(tag: string): ConnectHookElement {
  if (customElements.get(tag) === undefined) {
    customElements.define(tag, connectHookClass());
  }
  return document.createElement(tag) as ConnectHookElement;
}
