// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FailureKindSchema } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { ForwardingLogger, setLogger } from "../../src/log.js";
import {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  staleBundle,
  workspaceGone,
} from "../../src/failure/sink.js";
import { clearClientFailures, standingClientFailure } from "../../src/rpc/link.js";
import { drawFailureCard, mountFailureOverlay } from "../../src/failure/overlay.js";

let host: HTMLElement;

beforeEach(() => {
  host = document.createElement("div");
  host.id = "failure-overlay";
  host.setAttribute("data-component", "failure-overlay");
  document.body.replaceChildren(host);
});

const cards = (): HTMLElement[] => Array.from(host.querySelectorAll<HTMLElement>(".failure-card"));
const arms = (): string[] => cards().map((c) => c.getAttribute("data-arm") ?? "");

describe("mountFailureOverlay: drawing an arm", () => {
  const all = [
    ["daemonUnreachable", () => daemonUnreachable(1006, "abnormal")],
    ["workspaceGone", () => workspaceGone()],
    ["bootFailed", () => bootFailed("Error: nope")],
    ["controlPlaneFailed", () => controlPlaneFailed("OpenLogin", "unavailable")],
    ["frameUndecodable", () => frameUndecodable("a oneof sets no arm", "FooterView")],
    ["staleBundle", () => staleBundle("schema drift")],
  ] as const;

  for (const [arm, mint] of all) {
    it(`draws a card for ${arm}`, () => {
      // ARRANGE
      const overlay = mountFailureOverlay(host);
      // ACT
      overlay.report(mint());
      // ASSERT
      expect(arms()).toEqual([arm]);
    });
  }

  for (const [arm, mint] of all) {
    it(`paints ${arm} blue, the client_local side's color`, () => {
      const overlay = mountFailureOverlay(host);
      overlay.report(mint());
      expect(cards()[0].classList.contains("tone-blue")).toBe(true);
    });
  }
});

describe("mountFailureOverlay: evidence", () => {
  it("shows the close code verbatim", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "abnormal"));
    expect(host.textContent).toContain("1006");
  });

  it("shows the close reason verbatim", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "abnormal closure"));
    expect(host.textContent).toContain("abnormal closure");
  });

  it("omits an empty evidence value rather than drawing a blank row", () => {
    // ARRANGE: the proto states outright that close_reason may be empty.
    const overlay = mountFailureOverlay(host);
    // ACT
    overlay.report(daemonUnreachable(1006, ""));
    // ASSERT: the code row only.
    expect(host.querySelectorAll(".failure-detail")).toHaveLength(1);
  });

  it("draws no evidence rows for an arm that carries none", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(workspaceGone());
    expect(host.querySelectorAll(".failure-detail")).toHaveLength(0);
  });

  it("shows both of controlPlaneFailed's fields, so two requests stay apart", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(controlPlaneFailed("OpenLogin", "unavailable"));
    expect(host.textContent).toContain("OpenLogin");
    expect(host.textContent).toContain("unavailable");
  });

  it("shows the frame head, for whoever debugs the skipped frame", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(frameUndecodable("cause", "WatchFooterResponse at .footer"));
    expect(host.textContent).toContain("WatchFooterResponse at .footer");
  });

  it("sets evidence as TEXT, so a hostile string cannot become markup", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("<img src=x onerror=alert(1)>"));
    expect(host.querySelector("img")).toBeNull();
  });
});

describe("mountFailureOverlay: reconciliation by arm", () => {
  it("stacks two DIFFERENT arms", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.report(staleBundle("b"));
    expect(arms()).toEqual(["daemonUnreachable", "staleBundle"]);
  });

  it("REPLACES the card when the same arm reports again", () => {
    // ARRANGE: a reconnect loop must not append a card per attempt.
    const overlay = mountFailureOverlay(host);
    // ACT
    overlay.report(daemonUnreachable(1006, "first"));
    overlay.report(daemonUnreachable(1000, "second"));
    // ASSERT
    expect(cards()).toHaveLength(1);
  });

  it("shows the LATEST evidence after a replacement", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "first"));
    overlay.report(daemonUnreachable(1000, "second"));
    expect(host.textContent).toContain("second");
  });

  it("drops the superseded evidence", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "first"));
    overlay.report(daemonUnreachable(1000, "second"));
    expect(host.textContent).not.toContain("first");
  });

  it("keeps a repeat of one arm from disturbing another's card", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("standing"));
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.report(daemonUnreachable(1000, "b"));
    expect(arms()).toEqual(["staleBundle", "daemonUnreachable"]);
  });
});

describe("mountFailureOverlay: retraction", () => {
  it("removes the arm's card", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.retract("daemonUnreachable");
    expect(cards()).toHaveLength(0);
  });

  it("leaves the other arms standing", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.report(staleBundle("b"));
    overlay.retract("daemonUnreachable");
    expect(arms()).toEqual(["staleBundle"]);
  });

  it("is a no-op for an arm with no card", () => {
    const overlay = mountFailureOverlay(host);
    expect(() => overlay.retract("daemonUnreachable")).not.toThrow();
  });

  it("lets workspaceGone stand, since nothing retracts it", () => {
    // ARRANGE: unlike a dropped connection there is nothing to come back.
    const overlay = mountFailureOverlay(host);
    overlay.report(workspaceGone());
    overlay.retract("daemonUnreachable");
    expect(arms()).toEqual(["workspaceGone"]);
  });

  it("lets staleBundle stand, which is deliberately unresolvable", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("drift"));
    overlay.retract("daemonUnreachable");
    expect(arms()).toEqual(["staleBundle"]);
  });

  it("re-reports an arm after it was retracted", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.retract("daemonUnreachable");
    overlay.report(daemonUnreachable(1006, "b"));
    expect(cards()).toHaveLength(1);
  });
});

describe("mountFailureOverlay: the empty state", () => {
  it("marks itself empty on mount, so the region costs nothing while healthy", () => {
    mountFailureOverlay(host);
    expect(host.hasAttribute("data-empty")).toBe(true);
  });

  it("drops the marker once a card is filed", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("x"));
    expect(host.hasAttribute("data-empty")).toBe(false);
  });

  it("re-marks itself empty when the last card is retracted", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.retract("daemonUnreachable");
    expect(host.hasAttribute("data-empty")).toBe(true);
  });
});

describe("mountFailureOverlay: refusals", () => {
  it("refuses a FailureKind that sets no arm", () => {
    const overlay = mountFailureOverlay(host);
    expect(() => overlay.report(create(FailureKindSchema, {}))).toThrow(MalformedView);
  });

  it("draws no card for a DAEMON-minted arm, which a frontend may not mint", () => {
    // ARRANGE
    const overlay = mountFailureOverlay(host);
    const foreign = create(FailureKindSchema, {
      kind: { case: "shimDegraded", value: { component: "stdout" } },
    });
    // ACT
    overlay.report(foreign);
    // ASSERT
    expect(cards()).toHaveLength(0);
  });

  it("logs a foreign arm at error rather than swallowing it", () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const overlay = mountFailureOverlay(host);
    overlay.report(
      create(FailureKindSchema, { kind: { case: "shimDegraded", value: { component: "stdout" } } }),
    );
    expect(
      lines.some(([level, line]) => level === "error" && line.includes("failure-overlay.foreign-arm")),
    ).toBe(true);
  });
});

describe("mountFailureOverlay: dispose", () => {
  it("clears the host", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.dispose();
    expect(host.children).toHaveLength(0);
  });

  it("forgets the cards, so a remount does not resurrect them", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "a"));
    overlay.dispose();
    overlay.retract("daemonUnreachable");
    expect(cards()).toHaveLength(0);
  });
});

describe("mountFailureOverlay: the card's shape", () => {
  it("uses a glyph rather than an emoji for the mark", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("x"));
    const mark = host.querySelector(".failure-mark")?.textContent ?? "";
    // ARRANGE/ACT/ASSERT: no code point in the emoji planes.
    expect([...mark].every((ch) => (ch.codePointAt(0) ?? 0) < 0x1f000)).toBe(true);
  });

  it("draws a headline chosen by the arm", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(workspaceGone());
    expect(host.querySelector(".failure-message")?.textContent).toContain("no longer exists");
  });

  it("announces itself to assistive technology without stealing focus", () => {
    const overlay = mountFailureOverlay(host);
    overlay.report(staleBundle("x"));
    expect(cards()[0].getAttribute("role")).toBe("status");
  });
});


describe("mountFailureOverlay: suppress", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  const NOW = 1_700_000_000_000;

  it("withholds the card while the announced window stands", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const overlay = mountFailureOverlay(host);
    // ACT
    overlay.suppress("daemonUnreachable", NOW + 8000);
    overlay.report(daemonUnreachable(1006, "gone"));
    // ASSERT
    expect(arms()).toEqual([]);
  });

  it("draws normally once the window has expired, since an overrun outage is news", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const overlay = mountFailureOverlay(host);
    overlay.suppress("daemonUnreachable", NOW + 8000);
    // ACT
    vi.setSystemTime(NOW + 8001);
    overlay.report(daemonUnreachable(1006, "gone"));
    // ASSERT
    expect(arms()).toEqual(["daemonUnreachable"]);
  });

  it("takes down a card already standing for the arm it starts suppressing", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const overlay = mountFailureOverlay(host);
    overlay.report(daemonUnreachable(1006, "gone"));
    // ACT
    overlay.suppress("daemonUnreachable", NOW + 8000);
    // ASSERT
    expect(arms()).toEqual([]);
  });

  it("suppresses only the arm it names", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const overlay = mountFailureOverlay(host);
    overlay.suppress("daemonUnreachable", NOW + 8000);
    // ACT
    overlay.report(bootFailed("no shell"));
    // ASSERT
    expect(arms()).toEqual(["bootFailed"]);
  });

  it("clears the suppression on retract, so an early recovery is not muted on", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const overlay = mountFailureOverlay(host);
    overlay.suppress("daemonUnreachable", NOW + 8000);
    // ACT
    overlay.retract("daemonUnreachable");
    overlay.report(daemonUnreachable(1006, "gone"));
    // ASSERT
    expect(arms()).toEqual(["daemonUnreachable"]);
  });
});

describe("drawFailureCard: evidence for an arm it has no rows for", () => {
  it("draws the card's headline with no evidence rows at all", () => {
    // ARRANGE — a DAEMON-minted arm, which `evidenceRows` deliberately has no
    // case for: neither producer may set the other's arms.
    const foreign = create(FailureKindSchema, {
      kind: { case: "shimDegraded", value: { component: "stdout" } },
    });
    // ACT
    const card = drawFailureCard(foreign, "bootFailed");
    // ASSERT
    expect(card.querySelectorAll(".failure-detail")).toHaveLength(0);
  });
});

describe("mountFailureOverlay and the client's link verdict", () => {
  afterEach(() => {
    clearClientFailures();
  });

  it("relays the daemonUnreachable card to the footer", () => {
    mountFailureOverlay(host).report(daemonUnreachable(1006, "abnormal"));
    expect(standingClientFailure()).toEqual({
      kind: "daemon_unreachable_card",
      substatus: "daemon unreachable",
      activity: "lost the connection to the daemon; reconnecting",
    });
  });

  it("relays the frameUndecodable card as a frame this page could not read", () => {
    mountFailureOverlay(host).report(frameUndecodable("a oneof sets no arm", "FooterView"));
    expect(standingClientFailure()).toEqual({
      kind: "frame_undecodable_card",
      substatus: "frame unreadable",
      activity: "a frame could not be read and was skipped, so conversation may be missing",
    });
  });

  it("relays NOTHING for an arm that is not about this page's link", () => {
    mountFailureOverlay(host).report(staleBundle("schema drift"));
    expect(standingClientFailure()).toBeNull();
  });

  it("relays nothing while the card is suppressed for an announced outage", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1_800_000_000_000);
    const overlay = mountFailureOverlay(host);
    overlay.suppress("daemonUnreachable", Date.now() + 60_000);
    overlay.report(daemonUnreachable(1006, "abnormal"));
    vi.useRealTimers();
    expect(standingClientFailure()).toBeNull();
  });
});
