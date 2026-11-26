// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FailureKindSchema } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  frameUndrawable,
  staleBundle,
  workspaceGone,
} from "../../src/failure/sink.js";
import { clearClientFailures, standingClientFailure } from "../../src/rpc/link.js";
import { ARM_HEADLINE, createLocalFailures, evidenceContext, evidenceRows } from "../../src/failure/local.js";
import { captureLogRecords, forwardedRecord, forwardedRecords, type LogCapture } from "../log-capture.js";

const NOW = 1_700_000_000_000;

afterEach(() => {
  vi.useRealTimers();
  clearClientFailures();
});

const MINTS = [
  ["daemonUnreachable", () => daemonUnreachable(1006, "abnormal")],
  ["workspaceGone", () => workspaceGone()],
  ["bootFailed", () => bootFailed("Error: nope")],
  ["controlPlaneFailed", () => controlPlaneFailed("OpenLogin", "unavailable")],
  ["frameUndecodable", () => frameUndecodable("a oneof sets no arm", "FooterView")],
  ["staleBundle", () => staleBundle("schema drift")],
  ["frameUndrawable", () => frameUndrawable("WatchFeed", "TypeError: boom")],
] as const;

/** Every forwarded record under OPERATION, once the batch has flushed. */
async function recordsUnder(capture: LogCapture, operation: string) {
  return (await forwardedRecords(capture)).filter((record) => record.operation === operation);
}

describe("createLocalFailures: filing an arm", () => {
  for (const [arm, mint] of MINTS) {
    it(`records ${arm} at error, stated with its arm's sentence`, async () => {
      // ARRANGE
      const capture = captureLogRecords("debug");
      const failures = createLocalFailures();
      // ACT
      failures.report(mint());
      // ASSERT
      const record = await forwardedRecord(capture, "failure.report");
      expect([record.level.case, record.message]).toEqual(["error", `filing the ${arm} failure: ${ARM_HEADLINE[arm]}`]);
    });
  }

  it("carries the arm's own evidence on its record", async () => {
    const capture = captureLogRecords("debug");
    createLocalFailures().report(daemonUnreachable(1006, "abnormal"));
    expect((await forwardedRecord(capture, "failure.report")).context).toEqual(
      expect.objectContaining({ close_code: "1006", close_reason: "abnormal" }),
    );
  });
});

describe("evidenceRows", () => {
  it("carries the close code verbatim", () => {
    expect(evidenceRows(daemonUnreachable(1006, "abnormal"))).toContainEqual(["close code", "1006"]);
  });

  it("carries the close reason verbatim", () => {
    expect(evidenceRows(daemonUnreachable(1006, "abnormal closure"))).toContainEqual([
      "close reason",
      "abnormal closure",
    ]);
  });

  it("omits an empty evidence value rather than keeping a blank row", () => {
    // ARRANGE / ACT: the proto states outright that close_reason may be empty.
    const rows = evidenceRows(daemonUnreachable(1006, ""));
    // ASSERT: the code row only.
    expect(rows).toEqual([["close code", "1006"]]);
  });

  it("has no rows for an arm that carries none", () => {
    expect(evidenceRows(workspaceGone())).toEqual([]);
  });

  it("keeps both of controlPlaneFailed's fields, so two requests stay apart", () => {
    expect(evidenceRows(controlPlaneFailed("OpenLogin", "unavailable"))).toEqual([
      ["request", "OpenLogin"],
      ["cause", "unavailable"],
    ]);
  });

  it("keeps the stream and the cause of a frame this page could not draw", () => {
    expect(evidenceRows(frameUndrawable("WatchFeed", "TypeError: boom"))).toEqual([
      ["stream", "WatchFeed"],
      ["cause", "TypeError: boom"],
    ]);
  });

  it("keeps the frame head, for whoever debugs the skipped frame", () => {
    expect(evidenceRows(frameUndecodable("cause", "WatchFooterResponse at .footer"))).toContainEqual([
      "frame",
      "WatchFooterResponse at .footer",
    ]);
  });

  it("has no rows for a daemon-minted arm, which a frontend may not mint", () => {
    const foreign = create(FailureKindSchema, {
      kind: { case: "shimDegraded", value: { component: "stdout" } },
    });
    expect(evidenceRows(foreign)).toEqual([]);
  });
});

describe("evidenceContext", () => {
  it("keys each evidence value by its label, spaces as underscores", () => {
    expect(evidenceContext(controlPlaneFailed("OpenLogin", "unavailable"))).toEqual({ request: "OpenLogin", cause: "unavailable" });
  });

  it("is the evidence rows, so a record and the rows never differ", () => {
    const kind = daemonUnreachable(1006, "abnormal closure");
    expect(Object.values(evidenceContext(kind))).toEqual(evidenceRows(kind).map(([, value]) => value));
  });
});

describe("createLocalFailures: one standing failure per arm", () => {
  it("records a repeat of a standing arm at debug, not as a second error", async () => {
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(daemonUnreachable(1006, "b"));
    expect([(await recordsUnder(capture, "failure.report")).length, (await recordsUnder(capture, "failure.replace")).length]).toEqual([1, 1]);
  });

  it("records two DIFFERENT arms each at error", async () => {
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(staleBundle("drift"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(2);
  });
});

describe("createLocalFailures: retraction", () => {
  it("records the retraction of a standing arm", async () => {
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    failures.retract("daemonUnreachable");
    expect(await recordsUnder(capture, "failure.retract")).toHaveLength(1);
  });

  it("records nothing for an arm that does not stand", async () => {
    const capture = captureLogRecords("debug");
    createLocalFailures().retract("daemonUnreachable");
    expect(await recordsUnder(capture, "failure.retract")).toHaveLength(0);
  });

  it("re-files an arm after it was retracted, at error again", async () => {
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.retract("daemonUnreachable");
    failures.report(daemonUnreachable(1006, "b"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(2);
  });
});

describe("createLocalFailures: refusals", () => {
  it("refuses a FailureKind that sets no arm", () => {
    const failures = createLocalFailures();
    expect(() => failures.report(create(FailureKindSchema, {}))).toThrow(MalformedView);
  });

  it("records a foreign arm at error and files nothing", async () => {
    const capture = captureLogRecords("debug");
    createLocalFailures().report(
      create(FailureKindSchema, { kind: { case: "shimDegraded", value: { component: "stdout" } } }),
    );
    expect((await forwardedRecord(capture, "failure.foreign-arm")).level.case).toBe("error");
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(0);
  });
});

describe("createLocalFailures: dispose", () => {
  it("forgets what stood, so a later report is an error again", async () => {
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.dispose();
    failures.report(daemonUnreachable(1006, "b"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(2);
  });
});

describe("createLocalFailures: suppress", () => {
  it("records a report it withholds at info, with its evidence", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    failures.report(daemonUnreachable(1006, "abnormal"));
    const record = await forwardedRecord(capture, "failure.suppressed");
    expect([record.level.case, record.context?.close_code, (await recordsUnder(capture, "failure.report")).length]).toEqual(["info", "1006", 0]);
  });

  it("files normally once the window has expired, since an overrun outage is news", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    vi.setSystemTime(NOW + 9_000);
    failures.report(daemonUnreachable(1006, "abnormal"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(1);
  });

  it("stands down a failure already standing for the arm it starts suppressing", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "before"));
    failures.suppress("daemonUnreachable", NOW + 8_000);
    vi.setSystemTime(NOW + 9_000);
    failures.report(daemonUnreachable(1006, "after"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(2);
  });

  it("suppresses only the arm it names", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    failures.report(staleBundle("drift"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(1);
  });

  it("keeps the announced window standing across a retract, so a handover's next stream end stays expected", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords("debug");
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    failures.retract("daemonUnreachable");
    failures.report(daemonUnreachable(1006, "next stream"));
    expect(await recordsUnder(capture, "failure.report")).toHaveLength(0);
  });

  it("answers suppressing only while the window stands, and only for its arm", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    const during = [failures.suppressing("daemonUnreachable"), failures.suppressing("staleBundle")];
    vi.setSystemTime(NOW + 9_000);
    expect([...during, failures.suppressing("daemonUnreachable")]).toEqual([true, false, false]);
  });
});

describe("createLocalFailures and the client's link verdict", () => {
  it("relays daemonUnreachable to the footer", () => {
    createLocalFailures().report(daemonUnreachable(1006, "abnormal"));
    expect(standingClientFailure()).toEqual({
      kind: "daemon_unreachable_card",
      substatus: "daemon unreachable",
      activity: "lost the connection to the daemon; reconnecting",
    });
  });

  it("relays frameUndecodable as a frame this page could not read", () => {
    createLocalFailures().report(frameUndecodable("a oneof sets no arm", "FooterView"));
    expect(standingClientFailure()).toEqual({
      kind: "frame_undecodable_card",
      substatus: "frame unreadable",
      activity: "a frame could not be read and was skipped, so conversation may be missing",
    });
  });

  it("relays frameUndrawable as the page's own failure, never the daemon's", () => {
    createLocalFailures().report(frameUndrawable("WatchFeed", "TypeError: boom"));
    expect(standingClientFailure()).toEqual({
      kind: "frame_undrawable_card",
      substatus: "page draw failed",
      activity: "this page failed to draw a frame and is drawing the stream afresh",
    });
  });

  it("relays NOTHING for an arm that is not about this page's link", () => {
    createLocalFailures().report(staleBundle("schema drift"));
    expect(standingClientFailure()).toBeNull();
  });

  it("relays nothing while the arm is suppressed for an announced outage", () => {
    vi.useFakeTimers();
    vi.setSystemTime(1_800_000_000_000);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", Date.now() + 60_000);
    failures.report(daemonUnreachable(1006, "abnormal"));
    expect(standingClientFailure()).toBeNull();
  });
});

describe("createLocalFailures: standing arms", () => {
  it("answers the arms standing, in the order first filed", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("drift"));
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(staleBundle("again"));
    expect(failures.standingArms()).toEqual(["staleBundle", "daemonUnreachable"]);
  });

  it("drops a retracted arm", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.retract("daemonUnreachable");
    expect(failures.standingArms()).toEqual([]);
  });

  it("stands no arm the announced window withholds", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8_000);
    failures.report(daemonUnreachable(1006, "a"));
    expect(failures.standingArms()).toEqual([]);
  });
});
