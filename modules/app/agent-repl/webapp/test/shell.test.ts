// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { shellElements, type ShellElements } from "../src/shell.js";
import { shellHTML } from "./shell-html.js";

/**
 * The shell's ids paired with the ShellElements key each resolves to. This is
 * the test's own copy on purpose: a table imported from the module under test
 * would agree with a renamed id by construction, and the id names are the
 * contract `index.html` and the integration suite both hold.
 */
const MOUNTS: ReadonlyArray<readonly [keyof ShellElements, string]> = [
  ["sidebar", "ws-sidebar"],
  ["topbar", "topbar"],
  ["drainBanner", "drain-banner"],
  ["feedScroll", "feed-scroll"],
  ["feed", "feed"],
  ["holdTray", "hold-tray"],
  ["footer", "footer"],
  ["gateDock", "gate-dock"],
  ["composer", "composer"],
  ["loginOverlay", "login-overlay"],
  ["newsDigest", "news-digest"],
];

/** A document carrying every shell mount point except those in OMIT. */
function shellDoc(omit: readonly string[] = []): Document {
  const doc = document.implementation.createHTMLDocument("shell");
  for (const [, id] of MOUNTS) {
    if (omit.includes(id)) continue;
    const element = doc.createElement("div");
    element.id = id;
    doc.body.append(element);
  }
  return doc;
}

describe("shellElements", () => {
  it("resolves every mount point of a complete shell", () => {
    // Arrange
    const doc = shellDoc();
    // Act
    const shell = shellElements(doc);
    // Assert — each key holds the element carrying its id, not merely something.
    const resolved = MOUNTS.map(([key]) => [key, shell[key].id] as const);
    expect(resolved).toEqual(MOUNTS.map(([key, id]) => [key, id]));
  });

  it("returns live elements from the document rather than detached copies", () => {
    // Arrange
    const doc = shellDoc();
    // Act
    const shell = shellElements(doc);
    // Assert — a component drawing into a copy would render into nothing.
    expect(shell.feed).toBe(doc.getElementById("feed"));
  });

  for (const [key, id] of MOUNTS) {
    it(`throws naming #${id} when the ${key} mount is missing`, () => {
      // Arrange — every other mount is present, so only this id can be blamed.
      const doc = shellDoc([id]);
      // Act + Assert
      expect(() => shellElements(doc)).toThrow(`the page shell is missing #${id}`);
    });
  }

  it("names the FIRST missing id when several are gone", () => {
    // Arrange — the topbar and the footer are both absent; the topbar reads
    // first, and a fault report naming the later one would send a reader past
    // the earlier break.
    const doc = shellDoc(["topbar", "footer"]);
    // Act + Assert
    expect(() => shellElements(doc)).toThrow("the page shell is missing #topbar");
  });
});

describe("the shipped shell", () => {
  it("carries no failure overlay: the topbar's warning chip is where errors show", () => {
    // Arrange
    const doc = document.implementation.createHTMLDocument("shell");
    // Act
    doc.body.innerHTML = shellHTML();
    // Assert
    expect(doc.getElementById("failure-overlay")).toBeNull();
  });
});
