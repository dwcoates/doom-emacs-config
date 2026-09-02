// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";
import { mountRevealLayer, type RevealGeometry } from "../../src/topbar/reveal.js";

/** jsdom reports every rect as zero, so geometry is supplied outright. */
const GEOMETRY: RevealGeometry = {
  rectOf: () => ({ left: 0, top: 0, right: 0, bottom: 0, width: 0, height: 0 }),
  viewport: () => ({ width: 1000, height: 800 }),
};

let host: HTMLElement;

beforeEach(() => {
  host = document.createElement("div");
  document.body.replaceChildren(host);
});

/** An anchor the layer can find by name after a redraw. */
function anchor(name: string): HTMLElement {
  const button = document.createElement("button");
  button.setAttribute("data-reveal-anchor", name);
  host.append(button);
  return button;
}

const body = (text: string) => (): HTMLElement => {
  const element = document.createElement("div");
  element.textContent = text;
  return element;
};

const panels = (): string[] =>
  Array.from(host.querySelectorAll("[data-reveal]")).map(
    (el) => el.getAttribute("data-reveal") ?? "",
  );

describe("mountRevealLayer: opening", () => {
  it("ships with nothing open", () => {
    anchor("model");
    expect(mountRevealLayer(host, GEOMETRY).current()).toBeNull();
  });

  it("draws the named reveal under its anchor", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    expect(panels()).toEqual(["model"]);
  });

  it("holds at most one, so a second open replaces the first", () => {
    anchor("model");
    anchor("context");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    layer.open("context", "context", body("breakdown"));
    expect(panels()).toEqual(["context"]);
  });

  it("closes on a second toggle of the same reveal", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.toggle("model", "model", body("options"));
    layer.toggle("model", "model", body("options"));
    expect(layer.current()).toBeNull();
  });

  it("refuses to open a reveal whose anchor is not drawn", () => {
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("warnings", "warnings", body("list"));
    expect(layer.current()).toBeNull();
  });
});

describe("mountRevealLayer: surviving a push", () => {
  it("re-opens from the entry the latest draw registered", () => {
    // ARRANGE: the reader has the model list open.
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("old options"));
    // ACT: a push redraws the strip and re-registers with the new content.
    layer.register("model", "model", body("new options"));
    layer.refresh();
    // ASSERT
    expect(host.querySelector("[data-reveal]")?.textContent).toBe("new options");
  });

  it("closes the reveal when its anchor is gone from the new view", () => {
    // ARRANGE
    const chip = anchor("warnings");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("warnings", "warnings", body("list"));
    // ACT: the warnings cleared, so the chip is not drawn any more.
    chip.remove();
    layer.refresh();
    // ASSERT
    expect(layer.current()).toBeNull();
  });

  it("does nothing on a refresh with nothing open", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.register("model", "model", body("options"));
    layer.refresh();
    expect(panels()).toEqual([]);
  });
});

describe("mountRevealLayer: closing gestures", () => {
  it("closes on a click outside the layer", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    document.body.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(layer.current()).toBeNull();
  });

  it("stays open for a click inside the reveal", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    host
      .querySelector("[data-reveal]")
      ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(layer.current()).toBe("model");
  });

  it("leaves a click on an anchor to that anchor's own toggle", () => {
    const button = anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    button.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(layer.current()).toBe("model");
  });

  it("closes on Escape", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(layer.current()).toBeNull();
  });

  it("ignores a key that is not Escape", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.open("model", "model", body("options"));
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "a" }));
    expect(layer.current()).toBe("model");
  });
});

describe("mountRevealLayer: dispose", () => {
  it("stops answering the document's gestures", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    const spy = vi.spyOn(document, "removeEventListener");
    layer.dispose();
    expect(spy).toHaveBeenCalledTimes(2);
    spy.mockRestore();
  });

  it("takes the layer out of the host", () => {
    anchor("model");
    const layer = mountRevealLayer(host, GEOMETRY);
    layer.dispose();
    expect(host.querySelector(".topbar-reveal-layer")).toBeNull();
  });
});
