// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createPresence } from "../../src/presence.js";
import { createUnseenGate, type UnseenEnv } from "../../src/sidebar/unseen.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { roster } from "./harness.js";

const OWN = "ws-self";
const OTHER = "ws-other";

/** A document whose visibility the test sets, and a clock the test reads. */
function env(initial: DocumentVisibilityState): UnseenEnv & { show(): void; hide(): void } {
  const doc = document.implementation.createHTMLDocument("unseen");
  let state = initial;
  Object.defineProperty(doc, "visibilityState", { configurable: true, get: () => state });
  let clock = 0;
  return {
    doc,
    now: () => (clock += 5),
    show: () => {
      state = "visible";
      doc.dispatchEvent(new Event("visibilitychange"));
    },
    hide: () => {
      state = "hidden";
      doc.dispatchEvent(new Event("visibilitychange"));
    },
  };
}

/** A gate whose draws the test reads back, in order. */
function gate(initial: DocumentVisibilityState): {
  drawn: WorkspaceRoster[];
  e: ReturnType<typeof env>;
  g: ReturnType<typeof createUnseenGate>;
} {
  const drawn: WorkspaceRoster[] = [];
  const e = env(initial);
  const g = createUnseenGate(OWN, (r) => drawn.push(r), e);
  return { drawn, e, g };
}

describe("the unseen gate", () => {
  it("draws every push while the page is visible", () => {
    // Arrange
    const { drawn, g } = gate("visible");
    const pushes = [roster({ current: OTHER }), roster({ current: OTHER })];

    // Act
    for (const push of pushes) g.offer(push);

    // Assert
    expect(drawn).toEqual(pushes);
  });

  it("holds a push while the page is hidden and its workspace is not current", () => {
    // Arrange
    const { drawn, g } = gate("hidden");

    // Act
    g.offer(roster({ current: OTHER }));

    // Assert
    expect(drawn).toEqual([]);
    expect(g.held()).toBe(1);
  });

  it("draws every push while hidden when its workspace is current", () => {
    // Arrange: Emacs is not the frontmost application, so the page on screen
    // reports hidden; it is still the one a reader can see.
    const { drawn, g } = gate("hidden");
    const pushes = [roster({ current: OWN }), roster({ current: OWN })];

    // Act
    for (const push of pushes) g.offer(push);

    // Assert
    expect(drawn).toEqual(pushes);
  });

  it("draws only the newest held push when the page turns visible", () => {
    // Arrange
    const { drawn, e, g } = gate("hidden");
    const newest = roster({ current: OTHER });
    g.offer(roster({ current: OTHER }));
    g.offer(roster({ current: OTHER }));
    g.offer(newest);

    // Act
    e.show();

    // Assert
    expect(drawn).toEqual([newest]);
    expect(g.held()).toBe(0);
  });

  it("records the visible resume at info with how many pushes it skipped", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { e, g } = gate("hidden");
    g.offer(roster({ current: OTHER }));
    g.offer(roster({ current: OTHER }));
    g.offer(roster({ current: OTHER }));

    // Act
    e.show();

    // Assert
    const record = await forwardedRecord(capture, "sidebar.unseen-resumed");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ cause: "seen", skipped: 2, draw_ms: 5 });
  });

  it("draws the push that makes its workspace current, skipping every held one", () => {
    // Arrange
    const { drawn, g } = gate("hidden");
    g.offer(roster({ current: OTHER }));
    g.offer(roster({ current: OTHER }));
    const switched = roster({ current: OWN });

    // Act
    g.offer(switched);

    // Assert
    expect(drawn).toEqual([switched]);
    expect(g.held()).toBe(0);
  });

  it("records the current resume at info with every held push skipped", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { g } = gate("hidden");
    g.offer(roster({ current: OTHER }));
    g.offer(roster({ current: OTHER }));

    // Act
    g.offer(roster({ current: OWN }));

    // Assert
    const record = await forwardedRecord(capture, "sidebar.unseen-resumed");
    expect(record.context).toMatchObject({ cause: "current", skipped: 2 });
  });

  it("draws nothing on turning visible when nothing is held", () => {
    // Arrange
    const { drawn, e } = gate("hidden");

    // Act
    e.show();

    // Assert
    expect(drawn).toEqual([]);
  });

  it("holds a push again after the page goes hidden once more", () => {
    // Arrange
    const { drawn, e, g } = gate("visible");
    e.hide();

    // Act
    g.offer(roster({ current: OTHER }));

    // Assert
    expect(drawn).toEqual([]);
    expect(g.held()).toBe(1);
  });

  it("holds a push when the roster names no current workspace", () => {
    // Arrange
    const { drawn, g } = gate("hidden");

    // Act
    g.offer(roster({}));

    // Assert
    expect(drawn).toEqual([]);
  });

  it("draws nothing on a visibility change after it is disposed", () => {
    // Arrange
    const { drawn, e, g } = gate("hidden");
    g.offer(roster({ current: OTHER }));
    g.dispose();

    // Act
    e.show();

    // Assert
    expect(drawn).toEqual([]);
    expect(g.held()).toBe(0);
  });

  it("holds a push while its host does not show the page", () => {
    // Arrange
    const doc = document.implementation.createHTMLDocument("in-host");
    Object.defineProperty(doc, "visibilityState", { configurable: true, get: () => "visible" });
    const presence = createPresence(doc, true);
    const drawn: WorkspaceRoster[] = [];
    const g = createUnseenGate(OWN, (r) => drawn.push(r), { doc, now: () => 0, presence });

    // Act
    g.offer(roster({ current: OTHER }));

    // Assert
    expect(drawn).toEqual([]);
    presence.dispose();
  });

  it("draws the newest held push when its host shows the page", () => {
    // Arrange
    const doc = document.implementation.createHTMLDocument("in-host");
    Object.defineProperty(doc, "visibilityState", { configurable: true, get: () => "visible" });
    const presence = createPresence(doc, true);
    const drawn: WorkspaceRoster[] = [];
    const g = createUnseenGate(OWN, (r) => drawn.push(r), { doc, now: () => 0, presence });
    const newest = roster({ current: OTHER });
    g.offer(roster({ current: OTHER }));
    g.offer(newest);

    // Act
    presence.setShown(true);

    // Assert
    expect(drawn).toEqual([newest]);
    presence.dispose();
  });
});
