// @vitest-environment jsdom
// A page the host stops showing frees its older rows. See src/feed/unshown-trim.ts.
import { describe, expect, it } from "vitest";
import { createPresence } from "../../src/presence.js";
import { installUnshownTrim } from "../../src/feed/unshown-trim.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { recordingFailures } from "../failure-sink.js";

/** A hosted page's real presence, shown as SHOWN says, and a trim that counts its runs. */
function world(shown: boolean, trim: () => Promise<void> = () => Promise.resolve()) {
  const presence = createPresence(document, true);
  presence.setShown(shown);
  const failures = recordingFailures();
  let runs = 0;
  const uninstall = installUnshownTrim({
    presence,
    ctx: { failures },
    trim: () => {
      runs += 1;
      return trim();
    },
  });
  return {
    failures,
    uninstall,
    runs: () => runs,
    /** The host shows (true) or hides (false) the page. */
    set: (next: boolean) => presence.setShown(next),
    /** The document's visibility changes, the host's show untouched. */
    visibilityChanged: () => document.dispatchEvent(new Event("visibilitychange")),
  };
}

describe("installUnshownTrim", () => {
  it("trims when the host stops showing the page", () => {
    // Arrange
    const w = world(true);

    // Act
    w.set(false);

    // Assert
    expect(w.runs()).toBe(1);
  });

  it("does not trim when the host shows the page", () => {
    // Arrange
    const w = world(false);

    // Act
    w.set(true);

    // Assert
    expect(w.runs()).toBe(0);
  });

  it("does not trim on a change of the document's visibility alone", () => {
    // Arrange
    const w = world(true);

    // Act
    w.visibilityChanged();

    // Assert
    expect(w.runs()).toBe(0);
  });

  it("trims once per switch away, not again while the page stays hidden", () => {
    // Arrange
    const w = world(true);
    w.set(false);

    // Act
    w.set(false);

    // Assert
    expect(w.runs()).toBe(1);
  });

  it("trims again on the next switch away", () => {
    // Arrange
    const w = world(true);
    w.set(false);
    w.set(true);

    // Act
    w.set(false);

    // Assert
    expect(w.runs()).toBe(2);
  });

  it("stops trimming once uninstalled", () => {
    // Arrange
    const w = world(true);
    w.uninstall();

    // Act
    w.set(false);

    // Assert
    expect(w.runs()).toBe(0);
  });

  it("files a trim that met a malformed answer, rather than throwing it", async () => {
    // Arrange
    const w = world(true, () => Promise.reject(new MalformedView("FeedPage.result", "no arm")));

    // Act
    w.set(false);
    await Promise.resolve();
    await Promise.resolve();

    // Assert
    expect(w.failures.cases).toEqual(["frameUndecodable"]);
  });
});
