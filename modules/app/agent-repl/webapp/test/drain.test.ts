/**
 * drain — the scheduled-shutdown drain lease's global banner. One edge per
 * test (AAA).
 */
import { describe, expect, it } from "vitest";

import {
  DRAINING_BODY_CLASS,
  drainBannerHtml,
  drainHeadline,
  drainShimNote,
  drainingOf,
  holdLine,
  holdReason,
  workspaceShortName,
} from "../src/drain.js";
import type { ShutdownHold, ShutdownScheduleDraining } from "../src/frontend-proto.js";
import indexHtml from "../index.html?raw";
import css from "../src/styles.css?raw";

/** Body of the first brace-balanced block introduced by `marker`. */
function blockAfter(source: string, marker: string): string {
  const at = source.indexOf(marker);
  if (at === -1) throw new Error(`stylesheet has no ${marker} block`);
  const open = source.indexOf("{", at);
  let depth = 0;
  for (let i = open; i < source.length; i++) {
    if (source[i] === "{") depth++;
    else if (source[i] === "}" && --depth === 0) return source.slice(open + 1, i);
  }
  throw new Error(`unbalanced ${marker} block`);
}

const NOW = 1_700_000_000_000;

function hold(over: Partial<ShutdownHold> = {}): ShutdownHold {
  return {
    workspace: "/Users/dev/src/app",
    sessionId: "s-1",
    turn: { turnId: "t-1" },
    ...over,
  };
}

function lease(over: Partial<ShutdownScheduleDraining> = {}): ShutdownScheduleDraining {
  return {
    scheduleId: "sched-1",
    scheduledAtMs: NOW - 95_000,
    cause: "manual restart",
    stopShims: false,
    holds: [hold()],
    ...over,
  };
}

describe("drainingOf", () => {
  it("yields the lease from a draining view", () => {
    const view = { state: { case: "draining" as const, value: lease() } };
    expect(drainingOf(view)?.scheduleId).toBe("sched-1");
  });

  it("yields null from an idle view", () => {
    const view = { state: { case: "idle" as const, value: {} } };
    expect(drainingOf(view)).toBeNull();
  });
});

describe("workspaceShortName", () => {
  it("names a workspace by its last path segment", () => {
    expect(workspaceShortName("/Users/dev/src/app-infra")).toBe("app-infra");
  });

  it("ignores a trailing separator", () => {
    expect(workspaceShortName("/Users/dev/src/app/")).toBe("app");
  });

  it("keeps the raw value when the path has no nameable leaf", () => {
    expect(workspaceShortName("/")).toBe("/");
  });
});

describe("holdReason", () => {
  it("names a turn-only hold as a turn in flight", () => {
    expect(holdReason(hold())).toBe("turn in flight");
  });

  it("names a tasks-only hold by its count", () => {
    expect(holdReason(hold({ turn: undefined, tasks: { count: 3 } }))).toBe("3 live tasks");
  });

  it("singularizes a one-task hold", () => {
    expect(holdReason(hold({ turn: undefined, tasks: { count: 1 } }))).toBe("1 live task");
  });

  it("names BOTH reasons when a session holds a turn and tasks at once", () => {
    const both = hold({ tasks: { count: 2 } });
    expect(holdReason(both)).toBe("turn in flight, 2 live tasks");
  });
});

describe("holdLine", () => {
  it("puts the workspace short name before its reason", () => {
    expect(holdLine(hold())).toBe("app — turn in flight");
  });
});

describe("drainHeadline", () => {
  it("carries the daemon's free-text cause", () => {
    expect(drainHeadline(lease({ cause: "merge of ws-7 rebuilt the daemon" }), NOW)).toContain(
      "merge of ws-7 rebuilt the daemon",
    );
  });

  it("reports elapsed time since the lease was taken", () => {
    expect(drainHeadline(lease(), NOW)).toContain("1m 35s");
  });
});

describe("drainShimNote", () => {
  it("says sessions survive when the bounce preserves shims", () => {
    expect(drainShimNote(lease({ stopShims: false }))).toBe(
      "sessions are preserved across the bounce",
    );
  });

  it("says sessions restart when the bounce stops shims", () => {
    expect(drainShimNote(lease({ stopShims: true }))).toBe(
      "sessions will be stopped and restarted",
    );
  });
});

describe("drainBannerHtml", () => {
  it("renders nothing when no lease is held", () => {
    expect(drainBannerHtml(null, NOW)).toBe("");
  });

  it("renders the headline for a held lease", () => {
    expect(drainBannerHtml(lease(), NOW)).toContain("Daemon bounce scheduled");
  });

  it("enumerates every hold rather than summarizing them", () => {
    const html = drainBannerHtml(
      lease({
        holds: [
          hold(),
          hold({ workspace: "/Users/dev/src/docs", sessionId: "s-2", turn: undefined, tasks: { count: 2 } }),
        ],
      }),
      NOW,
    );
    expect(html).toContain("app — turn in flight");
    expect(html).toContain("docs — 2 live tasks");
  });

  it("keeps the full workspace path as the row's tooltip", () => {
    expect(drainBannerHtml(lease(), NOW)).toContain('title="/Users/dev/src/app"');
  });

  it("counts the workspaces the bounce is waiting on", () => {
    expect(drainBannerHtml(lease(), NOW)).toContain("Waiting on 1 workspace;");
  });

  it("escapes markup in the daemon's free-text cause", () => {
    const html = drainBannerHtml(lease({ cause: "<img src=x>" }), NOW);
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img");
  });

  it("escapes markup in a workspace path", () => {
    const html = drainBannerHtml(
      lease({ holds: [hold({ workspace: "/w/<b>evil</b>" })] }),
      NOW,
    );
    expect(html).not.toContain("<b>evil");
  });

  it("offers no dismiss control while the bounce is pending", () => {
    expect(drainBannerHtml(lease(), NOW)).not.toContain("<button");
  });
});

describe("DRAINING_BODY_CLASS", () => {
  it("names the document-wide marker the chrome paints against", () => {
    expect(DRAINING_BODY_CLASS).toBe("draining");
  });
});

describe("index.html drain slot", () => {
  it("mounts the banner slot the chrome renders into", () => {
    expect(indexHtml).toContain(`<div id="drain-banner"></div>`);
  });

  it("leaves the slot un-hidden so :empty alone governs its collapse", () => {
    expect(indexHtml).not.toContain(`<div id="drain-banner" hidden>`);
  });

  it("keeps the slot out of the topbar, above the feed it heads", () => {
    const headerEnd = indexHtml.indexOf("</header>");
    const feed = indexHtml.indexOf(`<main id="feed">`);
    const slot = indexHtml.indexOf(`id="drain-banner"`);
    expect([slot > headerEnd, slot < feed]).toEqual([true, true]);
  });
});

describe("drain stylesheet contract", () => {
  it("collapses the banner slot entirely when no bounce is scheduled", () => {
    expect(css).toContain("#drain-banner:empty { display: none; }");
  });

  it("outlines the banner rather than filling it like the ungated alarm", () => {
    // A pending bounce is an announcement, not an alarm; spending the alarm
    // dress on it would devalue the alarm.
    const banner = blockAfter(css, "#drain-banner {");
    expect(banner).toContain("border: 1px solid var(--merge-border)");
  });

  it("draws the lease bubble SOLID where the classifier card is dashed", () => {
    // The two parked-prompt cards must be distinguishable without reading a
    // badge, since they wait on entirely different things.
    const lease = blockAfter(css, ".queued-card.lease-card {");
    const queued = blockAfter(css, "\n.queued-card {");
    expect([lease.includes("border: 1px solid"), queued.includes("border: 1px dashed")]).toEqual([
      true,
      true,
    ]);
  });
});
