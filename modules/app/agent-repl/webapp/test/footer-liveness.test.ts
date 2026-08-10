import { describe, expect, it } from "vitest";

import {
  FooterLivenessLog,
  LIVENESS_LOG_INTERVAL_MS,
  resolveFooterLiveness,
} from "../src/footer-liveness.js";
import type { FooterParts, LivenessGap, LivenessLogSink } from "../src/footer-liveness.js";
import type { ClientLogContext } from "../src/protocol.js";
import type { ClientLogLevel } from "../src/wslog.js";
import type { ProgressInput } from "../src/state-adapter.js";

const NOW = Date.parse("2024-05-01T12:00:00.000Z");

/** A resolved progress view, defaulted to a quiet idle session. */
function progress(over: Partial<ProgressInput> = {}): ProgressInput {
  return {
    workspace: "/w",
    fence: "s1",
    turnStartedAtMs: 0,
    thinkingTokens: 0,
    inputTokens: 0,
    compacting: null,
    retrying: null,
    authenticating: null,
    hook: null,
    blocked: null,
    interrupt: null,
    rateLimited: null,
    rateLimitedWeekly: null,
    failure: null,
    expensiveTurn: null,
    accounting: null,
    pendingPermissions: 0,
    queueDepth: 0,
    liveTaskCount: 3,
    ...over,
  };
}

/** The raw parts of a busy monitoring workspace — the shape that used to lie. */
function parts(over: Partial<FooterParts> = {}): FooterParts {
  return {
    progress: progress(),
    renderState: "idle_async",
    mergeStatus: null,
    agents: [],
    tasks: [],
    items: [],
    timerLabel: "0:24",
    ...over,
  };
}

/** One record the log sink took. */
interface Record {
  level: ClientLogLevel;
  message: string;
  context: ClientLogContext;
}

/** A log sink that keeps what it was handed. */
function sink(): { records: Record[]; write: LivenessLogSink } {
  const records: Record[] = [];
  return {
    records,
    write: (level, message, context) => {
      records.push({ level, message, context });
    },
  };
}

describe("resolveFooterLiveness: the one place provenance is decided", () => {
  it("resolves live figures when the link is up and the workspace is wired", () => {
    // Arrange / Act
    const got = resolveFooterLiveness({ linkUp: true, wired: true }, parts());
    // Assert
    expect(got.provenance).toBe("live");
  });

  it("carries the resolved view on the live arm", () => {
    // Arrange
    const p = progress({ liveTaskCount: 7 });
    // Act
    const got = resolveFooterLiveness({ linkUp: true, wired: true }, parts({ progress: p }));
    // Assert
    expect(got.provenance === "live" && got.live.progress.liveTaskCount).toBe(7);
  });

  it("names the link when this page's socket is not current", () => {
    // Arrange / Act
    const got = resolveFooterLiveness({ linkUp: false, wired: true }, parts());
    // Assert
    expect(got.provenance === "unknown" && got.gap).toBe("link-down");
  });

  it("names the workspace when the daemon has no live session for it", () => {
    // Arrange / Act
    const got = resolveFooterLiveness({ linkUp: true, wired: false }, parts());
    // Assert
    expect(got.provenance === "unknown" && got.gap).toBe("workspace-unwired");
  });

  it("names the view when no ProgressView has ever landed", () => {
    // Arrange / Act
    const got = resolveFooterLiveness({ linkUp: true, wired: true }, parts({ progress: null }));
    // Assert
    expect(got.provenance === "unknown" && got.gap).toBe("view-absent");
  });

  it("reports the link ahead of the workspace when both are gone", () => {
    // Arrange — a dead socket makes the wired question unanswerable, so the
    // record must name the fact that actually explains the silence.
    const got = resolveFooterLiveness({ linkUp: false, wired: false }, parts());
    // Assert
    expect(got.provenance === "unknown" && got.gap).toBe("link-down");
  });

  it("hands back NO data at all on the unknown arm", () => {
    // Arrange / Act — this is the whole structural guarantee: a renderer on
    // this arm has nothing to paint a figure from.
    const got = resolveFooterLiveness({ linkUp: false, wired: true }, parts());
    // Assert
    expect(Object.keys(got).sort()).toEqual(["gap", "provenance"]);
  });
});

describe("FooterLivenessLog: loud, never silent, never a flood", () => {
  it("announces the gap once on the edge into it", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    // Act
    log.observe({ provenance: "unknown", gap: "link-down" }, NOW, "sess-1");
    // Assert
    expect(s.records).toHaveLength(1);
  });

  it("carries the reason as structured context", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    // Act
    log.observe({ provenance: "unknown", gap: "workspace-unwired" }, NOW, "sess-1");
    // Assert
    expect(s.records[0].context.liveness_gap).toBe("workspace-unwired");
  });

  it("suppresses the repeat while the same gap stands inside the window", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    const gap = { provenance: "unknown", gap: "link-down" } as const;
    // Act — the chrome cadence renders many times a second.
    log.observe(gap, NOW, "sess-1");
    log.observe(gap, NOW + 16, "sess-1");
    log.observe(gap, NOW + 32, "sess-1");
    // Assert
    expect(s.records).toHaveLength(1);
  });

  it("says so again once the rate-limit window has passed", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    const gap = { provenance: "unknown", gap: "link-down" } as const;
    // Act
    log.observe(gap, NOW, "sess-1");
    log.observe(gap, NOW + LIVENESS_LOG_INTERVAL_MS, "sess-1");
    // Assert
    expect(s.records).toHaveLength(2);
  });

  it("announces a DIFFERENT gap immediately, window or no window", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    // Act
    log.observe({ provenance: "unknown", gap: "link-down" }, NOW, "sess-1");
    log.observe({ provenance: "unknown", gap: "workspace-unwired" }, NOW + 16, "sess-1");
    // Assert
    expect(s.records.map((r) => r.context.liveness_gap)).toEqual([
      "link-down",
      "workspace-unwired",
    ]);
  });

  it("re-arms on recovery, so the next gap is not swallowed by the old window", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    const gap = { provenance: "unknown", gap: "link-down" } as const;
    const live = resolveFooterLiveness({ linkUp: true, wired: true }, parts());
    // Act
    log.observe(gap, NOW, "sess-1");
    log.observe(live, NOW + 16, "sess-1");
    log.observe(gap, NOW + 32, "sess-1");
    // Assert
    expect(s.records).toHaveLength(2);
  });

  it("says nothing at all while the figures are live", () => {
    // Arrange
    const s = sink();
    const log = new FooterLivenessLog(s.write);
    // Act
    log.observe(resolveFooterLiveness({ linkUp: true, wired: true }, parts()), NOW, "sess-1");
    // Assert
    expect(s.records).toHaveLength(0);
  });
});

describe("the brand: a figure cannot be rendered from an unknown provenance", () => {
  it("refuses a hand-built FooterInput at compile time", () => {
    // Arrange — the ONLY mint is `resolveFooterLiveness`, because the brand's
    // symbol is private to footer-liveness.ts. Remembered values therefore
    // cannot be dressed up as live ones anywhere in the program.
    // @ts-expect-error a FooterInput cannot be constructed outside the resolve
    const forged: import("../src/footer-liveness.js").FooterInput = parts();
    // Act / Assert — the type error above IS the assertion; the runtime check
    // only keeps the fixture honest.
    expect(forged.timerLabel).toBe("0:24");
  });

  it("refuses to widen the unknown arm into one carrying figures", () => {
    // Arrange / Act
    const gap: LivenessGap = "link-down";
    const forged: import("../src/footer-liveness.js").FooterLiveness = {
      provenance: "unknown",
      gap,
      // @ts-expect-error the unknown arm has no payload to attach figures to
      live: parts(),
    };
    // Assert
    expect(forged.provenance).toBe("unknown");
  });
});
