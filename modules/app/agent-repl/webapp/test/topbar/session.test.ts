// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarAgentReplSessionSchema,
  TopbarConnectivitySchema,
  TopbarSessionBeganEditorStartSchema,
  TopbarSessionBeganLoginSchema,
  type TopbarAgentReplSession,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  SESSION_REVEAL,
  bindAgentReplSessionReveal,
  drawAgentReplSession,
} from "../../src/topbar/session.js";
import { drawTopbarConnectivity } from "../../src/topbar/strip.js";
import { NOW, appContext, fakeTicker, openPanel, topbarContext } from "./fixtures.js";

const MINUTE = 60_000;

/** The two causes, as the generated arms carry them. */
const LOGIN = { case: "login" as const, value: create(TopbarSessionBeganLoginSchema, {}) };
const EDITOR_START = { case: "editorStart" as const, value: create(TopbarSessionBeganEditorStartSchema, {}) };

/** A session a login began 72 minutes before NOW. */
function session(overrides: Partial<TopbarAgentReplSession> = {}): TopbarAgentReplSession {
  const base = create(TopbarAgentReplSessionSchema, {
    startedAtMs: BigInt(NOW - 72 * MINUTE),
    began: { case: "login", value: {} },
  });
  return { ...base, ...overrides };
}

/** A glyph in a mounted host, its reveal bound, and the host's ticker. */
function boundGlyph(u: TopbarAgentReplSession | undefined) {
  const ticker = fakeTicker();
  const { host, tc } = topbarContext(appContext({}, undefined, ticker));
  const glyph = drawTopbarConnectivity(create(TopbarConnectivitySchema, { tone: "green", glyph: "●", title: "connected" }));
  host.append(glyph);
  bindAgentReplSessionReveal(glyph, u, tc);
  return { host, tc, glyph, ticker };
}

/** Click an element the way a reader does. */
function click(el: Element): void {
  el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
}

describe("bindAgentReplSessionReveal", () => {
  it("opens the session dropdown from the glyph", () => {
    // ARRANGE
    const { host, glyph } = boundGlyph(session());
    // ACT
    click(glyph);
    // ASSERT
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe(SESSION_REVEAL);
  });

  it("marks the glyph as the dropdown's anchor, so an outside click spares it", () => {
    const { glyph } = boundGlyph(session());
    expect(glyph.getAttribute("data-reveal-anchor")).toBe(SESSION_REVEAL);
  });

  it("closes the dropdown on a second click of the glyph", () => {
    // ARRANGE
    const { host, glyph } = boundGlyph(session());
    click(glyph);
    // ACT
    click(glyph);
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });

  it("keeps the glyph's click from reaching the cell around it", () => {
    // ARRANGE: the account cell's own click opens the account options.
    const { glyph } = boundGlyph(session());
    const cell = document.createElement("div");
    glyph.replaceWith(cell);
    cell.append(glyph);
    let reached = false;
    cell.addEventListener("click", () => {
      reached = true;
    });
    // ACT
    click(glyph);
    // ASSERT
    expect(reached).toBe(false);
  });

  it("re-opens a refreshed dropdown with the newer push's session", () => {
    // ARRANGE
    const { host, tc, glyph } = boundGlyph(session());
    click(glyph);
    // ACT
    bindAgentReplSessionReveal(glyph, session({ began: EDITOR_START }), tc);
    tc.reveals.refresh();
    // ASSERT
    expect(openPanel(host)?.querySelector(".topbar-session-duration .topbar-session-label")?.textContent).toBe(
      "since Emacs started",
    );
  });

  // NO SESSION, NO DROPDOWN: the glyph binds nothing and its click is the
  // cell's, as it was before there was a session to show.
  it("binds nothing when the view carries no session", () => {
    // ARRANGE
    const { host, glyph } = boundGlyph(undefined);
    // ACT
    click(glyph);
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });

  it("leaves a glyph with no session unmarked", () => {
    const { glyph } = boundGlyph(undefined);
    expect(glyph.getAttribute("data-reveal-anchor")).toBeNull();
  });

  it("lets a glyph with no session pass its click on to the cell", () => {
    // ARRANGE
    const { glyph } = boundGlyph(undefined);
    const cell = document.createElement("div");
    glyph.replaceWith(cell);
    cell.append(glyph);
    let reached = false;
    cell.addEventListener("click", () => {
      reached = true;
    });
    // ACT
    click(glyph);
    // ASSERT
    expect(reached).toBe(true);
  });
});

describe("drawAgentReplSession", () => {
  it("draws the session's span from its start against the reader's now", () => {
    // ARRANGE
    const { host, glyph } = boundGlyph(session());
    // ACT
    click(glyph);
    // ASSERT
    expect(openPanel(host)?.querySelector(".topbar-session-duration .topbar-session-value")?.textContent).toBe(
      "1h 12m",
    );
  });

  it("ticks the span locally as the reader's clock moves", () => {
    // ARRANGE
    const { host, glyph, ticker } = boundGlyph(session());
    click(glyph);
    // ACT
    ticker.set(NOW + 3 * MINUTE);
    // ASSERT
    expect(openPanel(host)?.querySelector(".topbar-session-duration .topbar-session-value")?.textContent).toBe(
      "1h 15m",
    );
  });

  it("draws the span as the dropdown's only row", () => {
    // ARRANGE
    const { host, glyph } = boundGlyph(session());
    // ACT
    click(glyph);
    // ASSERT
    expect(openPanel(host)?.querySelectorAll(".topbar-session-row").length).toBe(1);
  });

  it.each([
    ["login", LOGIN, "since login"],
    ["editorStart", EDITOR_START, "since Emacs started"],
  ])("labels the span by what began it: %s", (_name, began, want) => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const panel = drawAgentReplSession(session({ began }), tc);
    // ASSERT
    expect(panel.querySelector(".topbar-session-duration .topbar-session-label")?.textContent).toBe(want);
  });

  it("carries the cause as a hook", () => {
    const { tc } = topbarContext();
    const panel = drawAgentReplSession(session({ began: EDITOR_START }), tc);
    expect(panel.getAttribute("data-began")).toBe("editorStart");
  });

  it("refuses a session naming no cause", () => {
    const { tc } = topbarContext();
    expect(() => drawAgentReplSession(session({ began: { case: undefined } }), tc)).toThrow(MalformedView);
  });

  it("refuses a cause this build cannot draw", () => {
    // ARRANGE: a newer daemon's arm, reaching a build with no case for it.
    const { tc } = topbarContext();
    const future = session();
    (future.began as { case: string }).case = "deploy";
    // ACT / ASSERT
    expect(() => drawAgentReplSession(future, tc)).toThrow(/TopbarAgentReplSession.began.*deploy/);
  });

  it("refuses a start no clock can hold", () => {
    const { tc } = topbarContext();
    expect(() =>
      drawAgentReplSession(session({ startedAtMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n }), tc),
    ).toThrow(MalformedView);
  });
});
