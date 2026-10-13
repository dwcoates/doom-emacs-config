// @vitest-environment jsdom
// Whether a reader can see the page: shown by its host AND its document
// visible. See src/presence.ts.
import { afterEach, describe, expect, it } from "vitest";

import { createPresence, pageIsEmbedded, pagePresence, warmthCondition, type PresenceCause, type PresenceSource } from "../src/presence.js";

let presence: PresenceSource | null = null;

function setVisibility(state: "visible" | "hidden"): void {
  Object.defineProperty(document, "visibilityState", { configurable: true, get: () => state });
  document.dispatchEvent(new Event("visibilitychange"));
}

afterEach(() => {
  presence?.dispose();
  presence = null;
  setVisibility("visible");
});

describe("presence", () => {
  it.each([true, false])("says whether it sits in the webhost (%s)", (embedded) => {
    // Arrange / Act
    presence = createPresence(document, embedded);

    // Assert
    expect(presence.embedded).toBe(embedded);
  });

  it("is not seen in a host until the host shows it", () => {
    // Arrange / Act
    presence = createPresence(document, true);

    // Assert
    expect(presence.seen()).toBe(false);
  });

  it("is seen in no host while the document is visible", () => {
    // Arrange / Act
    presence = createPresence(document, false);

    // Assert
    expect(presence.seen()).toBe(true);
  });

  it("is seen once the host shows it", () => {
    // Arrange
    presence = createPresence(document, true);

    // Act
    presence.setShown(true);

    // Assert
    expect(presence.seen()).toBe(true);
  });

  it("is not seen while the document is hidden, shown or not", () => {
    // Arrange
    presence = createPresence(document, true);
    presence.setShown(true);

    // Act
    setVisibility("hidden");

    // Assert
    expect(presence.seen()).toBe(false);
  });

  it("tells a listener the host showed it", () => {
    // Arrange
    presence = createPresence(document, true);
    const told: [boolean, PresenceCause][] = [];
    presence.onChange((seen, cause) => told.push([seen, cause]));

    // Act
    presence.setShown(true);

    // Assert
    expect(told).toEqual([[true, "host"]]);
  });

  it("tells a listener the document's visibility changed", () => {
    // Arrange
    presence = createPresence(document, false);
    const told: [boolean, PresenceCause][] = [];
    presence.onChange((seen, cause) => told.push([seen, cause]));

    // Act
    setVisibility("hidden");

    // Assert
    expect(told).toEqual([[false, "visibility"]]);
  });

  it("tells nobody when the host repeats itself", () => {
    // Arrange
    presence = createPresence(document, true);
    const told: boolean[] = [];
    presence.onChange((seen) => told.push(seen));

    // Act
    presence.setShown(false);

    // Assert
    expect(told).toEqual([]);
  });

  it("stops telling a listener that unsubscribed", () => {
    // Arrange
    presence = createPresence(document, true);
    const told: boolean[] = [];
    const unsubscribe = presence.onChange((seen) => told.push(seen));

    // Act
    unsubscribe();
    presence.setShown(true);

    // Assert
    expect(told).toEqual([]);
  });
});

describe("pageIsEmbedded", () => {
  it("is false for a top-level window", () => {
    expect(pageIsEmbedded(window)).toBe(false);
  });

  it("is true for a window whose parent is another", () => {
    expect(pageIsEmbedded({ parent: window } as unknown as Window)).toBe(true);
  });
});

describe("pagePresence", () => {
  it("answers the same presence for the same page", () => {
    expect(pagePresence()).toBe(pagePresence());
  });

  it("answers a page's own presence for each document", () => {
    // Arrange
    const first = pagePresence();
    const other = document.implementation.createHTMLDocument("other page");
    const real = globalThis.document;
    Object.defineProperty(globalThis, "document", { configurable: true, value: other });

    // Act
    const second = pagePresence();
    Object.defineProperty(globalThis, "document", { configurable: true, value: real });

    // Assert
    expect(second).not.toBe(first);
  });
});

describe("warmth", () => {
  it("is cold in a host until the host warms it", () => {
    // Arrange / Act
    presence = createPresence(document, true);

    // Assert
    expect(presence.warm()).toBe(false);
  });

  it("is warm in no host", () => {
    // Arrange / Act
    presence = createPresence(document, false);

    // Assert
    expect(presence.warm()).toBe(true);
  });

  it("is warm while shown, whatever it was told of warmth", () => {
    // Arrange
    presence = createPresence(document, true);

    // Act
    presence.setShown(true);

    // Assert
    expect(presence.warm()).toBe(true);
  });

  it("is warm once the host warms it", () => {
    // Arrange
    presence = createPresence(document, true);

    // Act
    presence.setWarm(true);

    // Assert
    expect(presence.warm()).toBe(true);
  });

  it("tells its listeners when it warms", () => {
    // Arrange
    presence = createPresence(document, true);
    const causes: PresenceCause[] = [];
    presence.onChange((_seen, cause) => causes.push(cause));

    // Act
    presence.setWarm(true);

    // Assert
    expect(causes).toEqual(["host"]);
  });

  it("tells no one when its warmth does not change", () => {
    // Arrange
    presence = createPresence(document, true);
    const causes: PresenceCause[] = [];
    presence.onChange((_seen, cause) => causes.push(cause));

    // Act
    presence.setWarm(false);

    // Assert
    expect(causes).toEqual([]);
  });

  it("gives its standing views a condition that follows its warmth", () => {
    // Arrange
    presence = createPresence(document, true);
    const cond = warmthCondition(presence);
    let told = 0;
    cond.onChange(() => {
      told += 1;
    });

    // Act
    presence.setWarm(true);

    // Assert
    expect([cond.active(), told]).toEqual([true, 1]);
  });
});
