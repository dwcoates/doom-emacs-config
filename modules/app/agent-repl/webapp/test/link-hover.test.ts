// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { installLinkHoverPopup } from "../src/link-hover.js";

function mount(href: string): { root: HTMLElement; anchor: HTMLAnchorElement; off: () => void } {
  const root = document.createElement("div");
  const anchor = document.createElement("a");
  anchor.setAttribute("href", href);
  const inner = document.createElement("b");
  inner.textContent = "click here";
  anchor.appendChild(inner);
  root.appendChild(anchor);
  document.body.appendChild(root);
  return { root, anchor, off: installLinkHoverPopup(root) };
}

const popup = (): HTMLElement | null => document.querySelector(".link-hover-popup");
const fire = (el: Element, type: string): void => {
  el.dispatchEvent(new MouseEvent(type, { bubbles: true }));
};

afterEach(() => {
  document.body.innerHTML = "";
});

describe("installLinkHoverPopup", () => {
  it("shows exactly the url on hover", () => {
    const { anchor } = mount("https://example.test/a?b=1");
    fire(anchor.firstElementChild!, "mouseover");
    expect(popup()!.hidden).toBe(false);
    expect(popup()!.textContent).toBe("https://example.test/a?b=1");
  });

  it("hides on mouse out", () => {
    const { anchor } = mount("https://example.test/");
    fire(anchor, "mouseover");
    fire(anchor, "mouseout");
    expect(popup()!.hidden).toBe(true);
  });

  it("hides on click", () => {
    const { anchor } = mount("https://example.test/");
    fire(anchor, "mouseover");
    fire(anchor, "click");
    expect(popup()!.hidden).toBe(true);
  });

  it("ignores a non-http(s) href", () => {
    const { anchor } = mount("~/foo/bar.ts");
    fire(anchor, "mouseover");
    expect(popup()!.hidden).toBe(true);
  });

  it("removes the popup when uninstalled", () => {
    const { off } = mount("https://example.test/");
    off();
    expect(popup()).toBeNull();
  });
});
