// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarWarningSchema,
  TopbarWarningStripSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  clockTime,
  drawLocalWarningStrip,
  drawTopbarWarningStrip,
  drawWarningDetail,
  drawWarningList,
} from "../../src/topbar/warnings.js";
import { createLocalFailures, type LocalFailures } from "../../src/failure/local.js";
import { daemonUnreachable, staleBundle, workspaceGone } from "../../src/failure/sink.js";
import { oneofArms } from "../arms.js";
import { NOW, appContext, openPanel, topbarContext } from "./fixtures.js";

const warning = (text: string, detail: unknown) =>
  create(TopbarWarningSchema, { line: { text }, detail: detail as never });

const strip = (...warnings: ReturnType<typeof warning>[]) =>
  create(TopbarWarningStripSchema, { warnings });

const ACCOUNTING = { case: "accounting", value: { lines: [{ text: "2 responses missing usage" }] } };

/** Mount the chip and open its list. */
function openList(view: ReturnType<typeof strip>) {
  const { host, tc } = topbarContext();
  const chip = drawTopbarWarningStrip(view, tc);
  if (chip !== null) host.append(chip);
  host
    .querySelector(".topbar-warning-chip")
    ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  return host;
}

describe("drawTopbarWarningStrip", () => {
  it("draws NOTHING on an empty list, not a quiet chip", () => {
    const { tc } = topbarContext();
    expect(drawTopbarWarningStrip(strip(), tc)).toBeNull();
  });

  it("badges the count of the served list", () => {
    const host = openList(strip(warning("a", ACCOUNTING), warning("b", ACCOUNTING)));
    expect(host.querySelector(".topbar-warning-count")?.textContent).toBe("2");
  });

  it("lists each warning's composed sentence verbatim, in the served order", () => {
    const host = openList(strip(warning("newest", ACCOUNTING), warning("older", ACCOUNTING)));
    const rows = Array.from(openPanel(host)!.querySelectorAll(".topbar-warning-row")).map(
      (el) => el.textContent,
    );
    expect(rows).toEqual(["newest", "older"]);
  });

  it("carries each row's detail arm as a hook", () => {
    const host = openList(strip(warning("a", ACCOUNTING)));
    expect(openPanel(host)?.querySelector(".topbar-warning-row")?.getAttribute("data-arm")).toBe(
      "accounting",
    );
  });

  // A WARNING WITH NO DETAIL IS A STATEMENT. The workspace's session-less
  // state rides here since the whole-view states were retired: "hibernated
  // since 14:03", "cold context, awaiting your answer". There is nothing
  // behind it to reveal, and the gate is answered in the feed's card.
  it("lists a warning with no detail as a statement row", () => {
    const host = openList(strip(warning("cold context, awaiting your answer", undefined)));
    const row = openPanel(host)?.querySelector(".topbar-warning-row");
    expect(row?.getAttribute("data-statement")).toBe("");
  });

  it("draws a statement row as text rather than a button that opens nothing", () => {
    const host = openList(strip(warning("hibernated since 14:03", undefined)));
    expect(openPanel(host)?.querySelector(".topbar-warning-row")?.tagName).toBe("DIV");
  });

  it("still draws the statement's sentence verbatim", () => {
    const host = openList(strip(warning("hibernated since 14:03", undefined)));
    expect(openPanel(host)?.querySelector(".topbar-warning-row")?.textContent).toBe(
      "hibernated since 14:03",
    );
  });

  it("refuses a warning carrying no line", () => {
    const { tc } = topbarContext();
    const bad = strip(create(TopbarWarningSchema, { detail: ACCOUNTING as never }));
    expect(() => drawWarningList(bad, tc)).toThrow(MalformedView);
  });
});

describe("the detail overlay", () => {
  /** Open the list, then the first row's detail. */
  function openDetail(view: ReturnType<typeof strip>) {
    const host = openList(view);
    openPanel(host)!
      .querySelector(".topbar-warning-row")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    return host;
  }

  it("opens the detail on a row click", () => {
    const host = openDetail(strip(warning("a", ACCOUNTING)));
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("warning-detail");
  });

  it("returns to the list from the back affordance", () => {
    const host = openDetail(strip(warning("a", ACCOUNTING)));
    openPanel(host)!
      .querySelector("[data-back]")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("warnings");
  });

  it("draws the accounting evidence lines", () => {
    const host = openDetail(strip(warning("a", ACCOUNTING)));
    expect(openPanel(host)?.textContent).toContain("2 responses missing usage");
  });

  it("draws an unmodeled tool's name and argument lines", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "unmodeledTool",
          value: { toolName: { text: "mcp__x__y" }, argumentLines: [{ text: "path=/tmp" }] },
        }),
      ),
    );
    expect(openPanel(host)?.textContent).toContain("path=/tmp");
  });

  it("draws the name alone when the daemon composed no argument line", () => {
    const host = openDetail(
      strip(
        warning("a", { case: "unmodeledTool", value: { toolName: { text: "mcp__x__y" } } }),
      ),
    );
    expect(openPanel(host)?.querySelector(".topbar-warning-lines")).toBeNull();
  });

  it("ticks a detached unmodeled tool's age", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "detachedUnmodeled",
          value: { toolName: { text: "runner" }, startedAtMs: BigInt(NOW - 90_000) },
        }),
      ),
    );
    expect(openPanel(host)?.querySelector(".topbar-warning-clock")?.textContent).toBe(
      "running 1m 30s",
    );
  });

  it("reads the tool clock's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the tool's start does not share the shared ticker's phase.
    const host = openDetail(
      strip(
        warning("a", {
          case: "detachedUnmodeled",
          value: { toolName: { text: "runner" }, startedAtMs: BigInt(NOW - 4920) },
        }),
      ),
    );
    // Assert: five real seconds of running reads 5s, not the lagging 4s.
    expect(openPanel(host)?.querySelector(".topbar-warning-clock")?.textContent).toBe("running 5s");
  });

  it("draws a session fault's component and detail", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "sessionFault",
          value: { component: { text: "watcher" }, detail: { text: "it stalled" } },
        }),
      ),
    );
    expect(openPanel(host)?.textContent).toContain("it stalled");
  });

  it("ticks an OPEN degraded window from when it began", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "degradedWindow",
          value: {
            component: { text: "watcher" },
            reason: { text: "backpressure" },
            beganAtMs: BigInt(NOW - 45_000),
            extent: { case: "open", value: {} },
          },
        }),
      ),
    );
    expect(openPanel(host)?.querySelector(".topbar-warning-span")?.textContent).toBe(
      "degraded since 45s",
    );
  });

  it("reads the degraded window's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the window's start does not share the shared ticker's phase.
    const host = openDetail(
      strip(
        warning("a", {
          case: "degradedWindow",
          value: {
            component: { text: "watcher" },
            reason: { text: "backpressure" },
            beganAtMs: BigInt(NOW - 4920),
            extent: { case: "open", value: {} },
          },
        }),
      ),
    );
    // Assert: five real seconds degraded reads 5s, not the lagging 4s.
    expect(openPanel(host)?.querySelector(".topbar-warning-span")?.textContent).toBe(
      "degraded since 5s",
    );
  });

  it("reports a CLOSED degraded window's span and what it cost", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "degradedWindow",
          value: {
            component: { text: "watcher" },
            reason: { text: "backpressure" },
            beganAtMs: BigInt(NOW - 45_000),
            extent: { case: "closed", value: { endedAtMs: BigInt(NOW), droppedCount: 7n } },
          },
        }),
      ),
    );
    expect(openPanel(host)?.querySelector(".topbar-warning-span")?.textContent).toBe(
      `${clockTime(NOW - 45_000)}–${clockTime(NOW)}, 7 observations dropped`,
    );
  });

  it("says 'observation' in the singular for one dropped", () => {
    const host = openDetail(
      strip(
        warning("a", {
          case: "degradedWindow",
          value: {
            component: { text: "watcher" },
            reason: { text: "backpressure" },
            beganAtMs: BigInt(NOW - 1000),
            extent: { case: "closed", value: { endedAtMs: BigInt(NOW), droppedCount: 1n } },
          },
        }),
      ),
    );
    expect(openPanel(host)?.querySelector(".topbar-warning-span")?.textContent).toContain(
      "1 observation dropped",
    );
  });

  it("refuses a degraded window naming no extent", () => {
    const { tc } = topbarContext();
    const bad = warning("a", {
      case: "degradedWindow",
      value: {
        component: { text: "watcher" },
        reason: { text: "backpressure" },
        beganAtMs: 0n,
      },
    });
    expect(() => drawWarningDetail(bad, tc, "TopbarWarning")).toThrow(MalformedView);
  });

  it("refuses a warning naming no detail arm", () => {
    const { tc } = topbarContext();
    expect(() =>
      drawWarningDetail(create(TopbarWarningSchema, { line: { text: "a" } }), tc, "TopbarWarning"),
    ).toThrow(MalformedView);
  });

  it("refuses a detail arm this build cannot draw, rather than drawing an empty overlay", () => {
    // ARRANGE: a newer daemon's detail arm reaching a build with no case for it.
    const { tc } = topbarContext();
    const future = warning("a", ACCOUNTING);
    (future.detail as { case: string }).case = "quotaExhausted";
    // ACT / ASSERT
    expect(() => drawWarningDetail(future, tc, "TopbarWarning")).toThrow(
      /TopbarWarning.detail.*quotaExhausted/,
    );
  });

  it("refuses a degraded window whose extent arm this build cannot draw", () => {
    // ARRANGE
    const { tc } = topbarContext();
    const future = warning("a", {
      case: "degradedWindow",
      value: {
        component: { text: "watcher" },
        reason: { text: "backpressure" },
        beganAtMs: BigInt(NOW - 1000),
        extent: { case: "closed", value: { endedAtMs: BigInt(NOW), droppedCount: 1n } },
      },
    });
    const detail = future.detail.value as { extent: { case: string } };
    detail.extent.case = "suspended";
    // ACT / ASSERT
    expect(() => drawWarningDetail(future, tc, "TopbarWarning")).toThrow(
      /degraded_window.extent.*suspended/,
    );
  });

  // ENUMERATED FROM THE SCHEMA: a detail arm added to the proto fails here.
  it("has a drawing for every detail arm the schema declares", () => {
    const drawn = ["accounting", "unmodeledTool", "detachedUnmodeled", "sessionFault", "degradedWindow"];
    expect(oneofArms(TopbarWarningSchema, "detail")).toEqual(drawn);
  });
});

/**
 * THE CLIENT-LOCAL FAILURES IN THE CHIP (owner ruling, 2026-09-23): the chip is
 * the one place an error shows, so the page's own failures are listed here,
 * ahead of the pushed warnings and counted in the same badge.
 */
describe("the chip's client-local failures", () => {
  /** A topbar context whose chip reads FAILURES. */
  function withFailures(failures: LocalFailures) {
    return topbarContext(appContext(), () => undefined, () => failures.standing());
  }

  /** Draw CHIP into HOST and open its list. */
  function openChip(host: HTMLElement, chip: HTMLElement | null): HTMLElement | null {
    if (chip !== null) host.append(chip);
    host
      .querySelector(".topbar-warning-chip")
      ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    return openPanel(host);
  }

  it("draws the chip over an EMPTY served list when a failure stands", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    const { tc } = withFailures(failures);
    expect(drawTopbarWarningStrip(strip(), tc)).not.toBeNull();
  });

  it("counts the served warnings and the standing failures in one badge", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    const { tc } = withFailures(failures);
    const chip = drawTopbarWarningStrip(strip(warning("a", ACCOUNTING)), tc);
    expect(chip?.querySelector(".topbar-warning-count")?.textContent).toBe("2");
  });

  it("lists the failures ahead of the served warnings", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawTopbarWarningStrip(strip(warning("served", ACCOUNTING)), tc));
    const rows = Array.from(panel?.querySelectorAll(".topbar-warning-row") ?? []).map(
      (row) => row.textContent,
    );
    expect(rows).toEqual(["this page cannot read the daemon's state", "served"]);
  });

  it("names the standing arms on the chip, readable without opening it", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    failures.report(daemonUnreachable(1006, "gone"));
    const { tc } = withFailures(failures);
    const chip = drawTopbarWarningStrip(strip(), tc);
    expect(chip?.getAttribute("data-local-arms")).toBe("staleBundle daemonUnreachable");
  });

  it("marks a failure's row as the page's own, by its arm", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    expect(panel?.querySelector("[data-local]")?.getAttribute("data-arm")).toBe("staleBundle");
  });

  it("lists a failure with no evidence as a statement", () => {
    const failures = createLocalFailures();
    failures.report(workspaceGone());
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    expect(panel?.querySelector("[data-local]")?.hasAttribute("data-statement")).toBe(true);
  });

  it("opens a failure's evidence behind its row", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "abnormal closure"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    panel?.querySelector("[data-local]")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    const lines = Array.from(
      openPanel(host)?.querySelectorAll(".topbar-warning-detail-line") ?? [],
    ).map((line) => line.textContent);
    expect(lines).toEqual(["close code: 1006", "close reason: abnormal closure"]);
  });

  it("heads a failure's evidence with the same headline its row carries", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    panel?.querySelector("[data-local]")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.querySelector(".topbar-warning-name")?.textContent).toBe(
      "lost the connection to the daemon; reconnecting",
    );
  });

  it("returns from a failure's evidence to the list", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    panel?.querySelector("[data-local]")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)
      ?.querySelector("[data-back]")
      ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("warnings");
  });

  it("falls back to the list when an open failure is retracted under it", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    failures.report(staleBundle("drift"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    panel?.querySelector("[data-local]")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    failures.retract("daemonUnreachable");
    tc.reveals.refresh();
    expect(openPanel(host)?.querySelector("[data-detail]")).toBeNull();
  });

  it("draws NOTHING before any push when no failure stands", () => {
    const { tc } = withFailures(createLocalFailures());
    expect(drawLocalWarningStrip(tc)).toBeNull();
  });

  it("badges the standing failures alone before any push", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    failures.report(workspaceGone());
    const { tc } = withFailures(failures);
    expect(drawLocalWarningStrip(tc)?.querySelector(".topbar-warning-count")?.textContent).toBe("2");
  });

  it("sets a failure's evidence as TEXT, so a hostile string cannot become markup", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("<img src=x onerror=alert(1)>"));
    const { host, tc } = withFailures(failures);
    const panel = openChip(host, drawLocalWarningStrip(tc));
    panel?.querySelector("[data-local]")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(host.querySelector("img")).toBeNull();
  });
});
