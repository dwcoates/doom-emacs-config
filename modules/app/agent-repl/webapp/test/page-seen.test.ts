// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { createPageSeen, type SeenCause } from "../src/page-seen.js";
import { createPresence } from "../src/presence.js";
import { visibilityDoc } from "./visibility-doc.js";

const OWN = "ws-self";
const OTHER = "ws-other";

/** A document whose visibility the test sets, firing the platform's event. */
const page = visibilityDoc;

describe("whether a reader can see the page", () => {
  it.each<{ name: string; visibility: DocumentVisibilityState; current: string | null; seen: boolean }>([
    { name: "a visible page with no current workspace", visibility: "visible", current: null, seen: true },
    { name: "a visible page whose workspace is not current", visibility: "visible", current: OTHER, seen: true },
    { name: "a hidden page whose workspace is current", visibility: "hidden", current: OWN, seen: true },
    { name: "a hidden page whose workspace is not current", visibility: "hidden", current: OTHER, seen: false },
    { name: "a hidden page before any roster", visibility: "hidden", current: null, seen: false },
  ])("answers $seen for $name", ({ visibility, current, seen }) => {
    // Arrange
    const p = page(visibility);
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));

    // Act
    if (current !== null) tracker.noteCurrent(current);

    // Assert
    expect(tracker.isSeen()).toBe(seen);
  });

  it("tells subscribers when the page turns unseen by hiding", () => {
    // Arrange
    const p = page("visible");
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));
    tracker.noteCurrent(OTHER);
    const told: [boolean, SeenCause][] = [];
    tracker.onChange((seen, cause) => told.push([seen, cause]));

    // Act
    p.set("hidden");

    // Assert
    expect(told).toEqual([[false, "presence"]]);
  });

  it("tells subscribers when the host shows a page it held", () => {
    // Arrange: an embedded page the host has not shown yet.
    const p = page("visible");
    const presence = createPresence(p.doc, true);
    const tracker = createPageSeen(OWN, presence);
    const told: [boolean, SeenCause][] = [];
    tracker.onChange((seen, cause) => told.push([seen, cause]));

    // Act
    presence.setShown(true);

    // Assert
    expect(told).toEqual([[true, "presence"]]);
  });

  it("tells subscribers when the page turns seen by becoming current", () => {
    // Arrange
    const p = page("hidden");
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));
    const told: [boolean, SeenCause][] = [];
    tracker.onChange((seen, cause) => told.push([seen, cause]));

    // Act
    tracker.noteCurrent(OWN);

    // Assert
    expect(told).toEqual([[true, "current"]]);
  });

  it("tells no one when a change leaves the page as seen as it was", () => {
    // Arrange: current, so hiding the page changes nothing a reader sees.
    const p = page("visible");
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));
    tracker.noteCurrent(OWN);
    const told: boolean[] = [];
    tracker.onChange((seen) => told.push(seen));

    // Act
    p.set("hidden");
    tracker.noteCurrent(OWN);

    // Assert
    expect(told).toEqual([]);
  });

  it("stops telling a subscriber once it unsubscribes", () => {
    // Arrange
    const p = page("visible");
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));
    const told: boolean[] = [];
    const unsubscribe = tracker.onChange((seen) => told.push(seen));

    // Act
    unsubscribe();
    p.set("hidden");

    // Assert
    expect(told).toEqual([]);
  });

  it("stops following the presence once disposed", () => {
    // Arrange
    const p = page("visible");
    const tracker = createPageSeen(OWN, createPresence(p.doc, false));
    const told: boolean[] = [];
    tracker.onChange((seen) => told.push(seen));

    // Act
    tracker.dispose();
    p.set("hidden");

    // Assert
    expect(told).toEqual([]);
    expect(tracker.isSeen()).toBe(true);
  });
});
