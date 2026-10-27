// @vitest-environment jsdom
/**
 * A REDRAW NEVER MOVES THE RAIL (owner request, 2026-10-27): every viewport's
 * position is read before a draw and put back after it, whether the draw kept
 * the viewport's element or drew it anew. jsdom keeps no scroll offset, so
 * `viewport` states one per element, clamped to a stated maximum as the
 * engine clamps it.
 */
import { afterEach, describe, expect, it } from "vitest";
import { resetLoggingForTests } from "../../src/log.js";
import { createRailScrollKeep } from "../../src/sidebar/scroll-keep.js";
import { captureLogRecords, forwardedRecord, forwardedRecords } from "../log-capture.js";

/** Give EL a scroll offset that clamps at MAX. */
function scrollable<T extends HTMLElement>(el: T, max = 1000): T {
  let top = 0;
  Object.defineProperty(el, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (v: number) => {
      top = Math.max(0, Math.min(v, max));
    },
  });
  return el;
}

/** A section of class CLS holding a rows region of class REGION, scrolling up to MAX. */
function section(cls: string, region: string, max = 1000): { section: HTMLElement; region: HTMLElement } {
  const el = document.createElement("div");
  el.className = cls;
  const rows = scrollable(document.createElement("div"), max);
  rows.className = region;
  el.appendChild(rows);
  return { section: el, region: rows };
}

/** A rail body scrolling up to MAX, holding SECTIONS. */
function railBody(sections: HTMLElement[], max = 1000): HTMLElement {
  const body = scrollable(document.createElement("div"), max);
  body.className = "sb-scroll";
  for (const s of sections) body.appendChild(s);
  return body;
}

afterEach(() => {
  resetLoggingForTests();
});

describe("createRailScrollKeep", () => {
  it("puts a section drawn anew back where the reader left the one it replaced", () => {
    // Arrange
    const old = section("open-section", "sb-sections");
    const body = railBody([old.section]);
    old.region.scrollTop = 140;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    const fresh = section("open-section", "sb-sections");
    old.section.replaceWith(fresh.section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    expect(fresh.region.scrollTop).toBe(140);
  });

  it("keeps each section under its own name, Deploys and Hidden Workspaces alike", () => {
    // Arrange
    const merged = section("merged-section", "rows");
    const hidden = section("hidden-section", "rows");
    const body = railBody([merged.section, hidden.section]);
    merged.region.scrollTop = 60;
    hidden.region.scrollTop = 20;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    const freshMerged = section("merged-section", "rows");
    const freshHidden = section("hidden-section", "rows");
    merged.section.replaceWith(freshMerged.section);
    hidden.section.replaceWith(freshHidden.section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    expect([freshMerged.region.scrollTop, freshHidden.region.scrollTop]).toEqual([60, 20]);
  });

  it("puts back a kept section the draw moved", () => {
    // Arrange
    const open = section("open-section", "sb-sections");
    const body = railBody([open.section]);
    open.region.scrollTop = 140;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    open.region.scrollTop = 0;
    // Act
    keep.restore(body, "workspaces");
    // Assert
    expect(open.region.scrollTop).toBe(140);
  });

  it("writes and logs nothing for a section the draw left where it was", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const open = section("open-section", "sb-sections");
    const body = railBody([open.section]);
    open.region.scrollTop = 140;
    let writes = 0;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    Object.defineProperty(open.region, "scrollTop", {
      configurable: true,
      get: () => 140,
      set: () => {
        writes += 1;
      },
    });
    // Act
    keep.restore(body, "workspaces");
    // Assert
    const records = await forwardedRecords(capture);
    expect([writes, records.filter((r) => r.operation === "sidebar.scroll-kept")]).toEqual([0, []]);
  });

  it("logs a restored section at INFO with where the draw left it and where it went back to", async () => {
    // Arrange
    const capture = captureLogRecords();
    const old = section("merged-section", "rows");
    const body = railBody([old.section]);
    old.region.scrollTop = 60;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    old.section.replaceWith(section("merged-section", "rows").section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.scroll-kept");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ viewport: "merged-section#0", drawn_at: 0, kept: 60, to: 60, clamped: false });
  });

  it("stops a section whose rows shrank where the box clamps it, and says so", async () => {
    // Arrange
    const capture = captureLogRecords();
    const old = section("open-section", "sb-sections");
    const body = railBody([old.section]);
    old.region.scrollTop = 140;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    const fresh = section("open-section", "sb-sections", 80);
    old.section.replaceWith(fresh.section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.scroll-kept");
    expect(fresh.region.scrollTop).toBe(80);
    expect(record.context).toMatchObject({ viewport: "open-section#0", drawn_at: 0, kept: 140, to: 80, clamped: true });
  });

  it("keeps the rail's body under the app it shows, so Chat's position is never put on the workspaces", () => {
    // Arrange
    const body = railBody([]);
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    body.scrollTop = 300;
    keep.capture(body, "chat");
    body.scrollTop = 0;
    // Act
    keep.restore(body, "workspaces");
    const onWorkspaces = body.scrollTop;
    keep.restore(body, "chat");
    // Assert
    expect([onWorkspaces, body.scrollTop]).toEqual([0, 300]);
  });

  it("keeps a section's position across a draw that does not show it", () => {
    // Arrange
    const open = section("open-section", "sb-sections");
    const body = railBody([open.section]);
    open.region.scrollTop = 140;
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    open.section.remove();
    keep.capture(body, "chat");
    keep.restore(body, "chat");
    const fresh = section("open-section", "sb-sections");
    body.appendChild(fresh.section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    expect(fresh.region.scrollTop).toBe(140);
  });

  it("puts back nothing for a section no capture has read", () => {
    // Arrange
    const body = railBody([]);
    const keep = createRailScrollKeep();
    keep.capture(body, "workspaces");
    const fresh = section("hidden-section", "rows");
    fresh.region.scrollTop = 40;
    body.appendChild(fresh.section);
    // Act
    keep.restore(body, "workspaces");
    // Assert
    expect(fresh.region.scrollTop).toBe(40);
  });
});
