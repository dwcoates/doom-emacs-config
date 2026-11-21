import { onTestFinished, vi } from "vitest";

/** The computed style each element a test overrode answers, for that test. */
const overridden = new Map<Element, CSSStyleDeclaration>();

/**
 * Answers STYLE as ELEMENT's computed style until the test ends; every other
 * element's is the engine's.
 *
 * ONE SPY SERVES EVERY OVERRIDE A TEST MAKES: a second `vi.spyOn` of the
 * same method returns the spy already installed, so a spy per override would
 * call itself as the "real" method and recurse without end.
 */
export function overrideComputedStyle(element: Element, style: CSSStyleDeclaration): void {
  if (overridden.size === 0) {
    const computed = window.getComputedStyle.bind(window);
    const spy = vi
      .spyOn(window, "getComputedStyle")
      .mockImplementation((el, pseudo) => overridden.get(el) ?? computed(el, pseudo));
    onTestFinished(() => {
      overridden.clear();
      spy.mockRestore();
    });
  }
  overridden.set(element, style);
}
