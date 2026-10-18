// The page's latencies, batched to the daemon until it acknowledges them. See
// src/latency-report.ts.
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";

import {
  ReportedLatencyKind,
  ReportLatenciesResponseSchema,
  type ReportedLatency,
  type ReportLatenciesResponse,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { WorkspaceRefSchema } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { installLatencyReport, MAX_PENDING_LATENCIES, REPORT_DELAY_MS, type LatencyReporter } from "../src/latency-report.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/work/ws-1" });

const recorded = (n: number): ReportLatenciesResponse =>
  create(ReportLatenciesResponseSchema, {
    result: { case: "success", value: { outcome: { case: "recorded", value: { recorded: n } } } },
  });

const refused = (): ReportLatenciesResponse =>
  create(ReportLatenciesResponseSchema, {
    result: {
      case: "error",
      value: { cause: { case: "workspaceRefMismatch", value: { workspaceId: "ws-1", reportedDir: "/x", registryDir: "/y" } } },
    },
  });

/** A page whose timers, stream attaches and daemon answers a test drives. */
class FakePage {
  readonly sent: ReportedLatency[][] = [];
  private timers: { ms: number; fn: () => void; live: boolean }[] = [];
  private attached = new Set<() => void>();
  private answers: (() => Promise<ReportLatenciesResponse>)[] = [];
  private releases: (() => void)[] = [];
  reporter!: LatencyReporter;

  install(): this {
    this.reporter = installLatencyReport({
      workspace: WORKSPACE,
      send: (batch) => {
        this.sent.push(batch);
        const answer = this.answers.shift() ?? (() => Promise.resolve(recorded(batch.length)));
        return answer();
      },
      after: (ms, fn) => {
        const timer = { ms, fn, live: true };
        this.timers.push(timer);
        return () => {
          timer.live = false;
        };
      },
      onAttached: (fn) => {
        this.attached.add(fn);
        return () => this.attached.delete(fn);
      },
    });
    return this;
  }

  /** The next send fails at the transport. */
  failNext(): void {
    this.answers.push(() => Promise.reject(new Error("connection refused")));
  }

  /** The next send is refused. */
  refuseNext(): void {
    this.answers.push(() => Promise.resolve(refused()));
  }

  /** The next send waits until the test releases it. */
  holdNext(): void {
    this.answers.push(
      () =>
        new Promise((resolve) => {
          this.releases.push(() => resolve(recorded(0)));
        }),
    );
  }

  release(): void {
    this.releases.shift()?.();
  }

  /** Fire every live timer. */
  async elapse(): Promise<void> {
    const due = this.timers.filter((t) => t.live);
    this.timers = [];
    for (const t of due) t.fn();
    await settle();
  }

  async attach(): Promise<void> {
    for (const fn of this.attached) fn();
    await settle();
  }

  liveTimers(): number[] {
    return this.timers.filter((t) => t.live).map((t) => t.ms);
  }
}

const settle = async (): Promise<void> => {
  for (let i = 0; i < 5; i++) await Promise.resolve();
};

const kinds = (batch: ReportedLatency[]): ReportedLatencyKind[] => batch.map((l) => l.kind);

describe("installLatencyReport", () => {
  it("sends the latencies recorded within the delay as one batch", async () => {
    // Arrange
    const page = new FakePage().install();

    // Act
    page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, 0.08);
    page.reporter.record(ReportedLatencyKind.PAGE_BOOT, 0.4);
    await page.elapse();

    // Assert
    expect(page.sent).toHaveLength(1);
    expect(kinds(page.sent[0])).toEqual([ReportedLatencyKind.SWITCH_REPEAT, ReportedLatencyKind.PAGE_BOOT]);
    expect(page.sent[0][0].workspace?.id).toBe("ws-1");
    expect(page.sent[0][0].seconds).toBe(0.08);
  });

  it("waits the report delay, and schedules one send for a burst", () => {
    // Arrange
    const page = new FakePage().install();

    // Act
    page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, 0.08);
    page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, 0.09);

    // Assert
    expect(page.liveTimers()).toEqual([REPORT_DELAY_MS]);
    expect(page.sent).toEqual([]);
  });

  it("keeps a batch whose call failed and sends it when the stream re-attaches", async () => {
    // Arrange
    const page = new FakePage().install();
    page.failNext();
    page.reporter.record(ReportedLatencyKind.SWITCH_FIRST, 0.7);
    await page.elapse();

    // Act
    await page.attach();

    // Assert
    expect(page.sent).toHaveLength(2);
    expect(kinds(page.sent[1])).toEqual([ReportedLatencyKind.SWITCH_FIRST]);
    expect(page.liveTimers()).toEqual([]);
  });

  it("never retries a failed call on a timer of its own", async () => {
    // Arrange
    const page = new FakePage().install();
    page.failNext();
    page.reporter.record(ReportedLatencyKind.SWITCH_FIRST, 0.7);

    // Act
    await page.elapse();

    // Assert
    expect(page.liveTimers()).toEqual([]);
  });

  it("drops a refused batch and records the refusal at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const page = new FakePage().install();
    page.refuseNext();
    page.reporter.record(ReportedLatencyKind.PAGE_BOOT, 0.4);
    await page.elapse();

    // Act
    await page.attach();

    // Assert
    expect(page.sent).toHaveLength(1);
    const record = await forwardedRecord(capture, "latency.report-refused");
    expect(record.level.case).toBe("error");
  });

  it("sends what was recorded while a call was out once the daemon answers", async () => {
    // Arrange
    const page = new FakePage().install();
    page.holdNext();
    page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, 0.08);
    await page.elapse();
    page.reporter.record(ReportedLatencyKind.PAGE_BOOT, 0.4);
    await page.elapse();

    // Act
    page.release();
    await settle();
    await page.elapse();

    // Assert
    expect(page.sent.map(kinds)).toEqual([[ReportedLatencyKind.SWITCH_REPEAT], [ReportedLatencyKind.PAGE_BOOT]]);
  });

  it("drops the oldest unsent latencies past its bound, and warns", async () => {
    // Arrange
    const capture = captureLogRecords();
    const page = new FakePage().install();

    // Act
    for (let i = 0; i <= MAX_PENDING_LATENCIES; i++) page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, i);
    await page.elapse();

    // Assert
    expect(page.sent[0]).toHaveLength(MAX_PENDING_LATENCIES);
    expect(page.sent[0][0].seconds).toBe(1);
    const record = await forwardedRecord(capture, "latency.report-overflow");
    expect(record.level.case).toBe("warn");
  });

  it("stops its scheduled send when disposed", async () => {
    // Arrange
    const page = new FakePage().install();
    page.reporter.record(ReportedLatencyKind.SWITCH_REPEAT, 0.08);

    // Act
    page.reporter.dispose();
    await page.elapse();
    await page.attach();

    // Assert
    expect(page.sent).toEqual([]);
  });
});
