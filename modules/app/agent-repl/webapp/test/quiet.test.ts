// @vitest-environment jsdom
// The lid-closed quiet state's per-document source. See src/quiet.ts.
import { describe, expect, it } from "vitest";

import { createQuiet, pageQuiet } from "../src/quiet.js";

describe("createQuiet", () => {
  it("starts not quiet", () => {
    // Arrange / Act
    const quiet = createQuiet();

    // Assert
    expect(quiet.quiet()).toBe(false);
  });

  it("tells each listener every change and nothing for a repeat", () => {
    // Arrange
    const quiet = createQuiet();
    const heard: boolean[] = [];
    quiet.onChange((q) => heard.push(q));

    // Act
    quiet.set(true);
    quiet.set(true);
    quiet.set(false);

    // Assert
    expect([heard, quiet.quiet()]).toEqual([[true, false], false]);
  });

  it("stops telling a listener that unsubscribed", () => {
    // Arrange
    const quiet = createQuiet();
    const heard: boolean[] = [];
    const stop = quiet.onChange((q) => heard.push(q));

    // Act
    stop();
    quiet.set(true);

    // Assert
    expect(heard).toEqual([]);
  });

  it("is one source per document", () => {
    // Act / Assert
    expect(pageQuiet(document)).toBe(pageQuiet(document));
  });
});
