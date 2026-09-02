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
  drawTopbarWarningStrip,
  drawWarningDetail,
  drawWarningList,
} from "../../src/topbar/warnings.js";
import { oneofArms } from "../arms.js";
import { NOW, openPanel, topbarContext } from "./fixtures.js";

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

  // ENUMERATED FROM THE SCHEMA: a detail arm added to the proto fails here.
  it("has a drawing for every detail arm the schema declares", () => {
    const drawn = ["accounting", "unmodeledTool", "detachedUnmodeled", "sessionFault", "degradedWindow"];
    expect(oneofArms(TopbarWarningSchema, "detail")).toEqual(drawn);
  });
});
