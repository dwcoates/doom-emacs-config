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
  staleBundle,
  workspaceGone,
} from "../../src/failure/sink.js";
import { clearClientFailures, standingClientFailure } from "../../src/rpc/link.js";
import { createLocalFailures, evidenceRows } from "../../src/failure/local.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "../log-capture.js";

const NOW = 1_700_000_000_000;

/** Every operation CAPTURE forwarded, once the batch has flushed. */
async function forwardedOperations(capture: LogCapture): Promise<string[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent.map((record) => record.operation);
}

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
] as const;

const armsOf = (failures: ReturnType<typeof createLocalFailures>): string[] =>
  failures.standing().map((failure) => failure.arm);

describe("createLocalFailures: filing an arm", () => {
  for (const [arm, mint] of MINTS) {
    it(`stands ${arm} once it is reported`, () => {
      // ARRANGE
      const failures = createLocalFailures();
      // ACT
      failures.report(mint());
      // ASSERT
      expect(armsOf(failures)).toEqual([arm]);
    });
  }

  it("starts empty, so a healthy page lists nothing", () => {
    expect(createLocalFailures().standing()).toEqual([]);
  });

  it("leads with a headline chosen by the arm", () => {
    const failures = createLocalFailures();
    failures.report(workspaceGone());
    expect(failures.standing()[0]?.headline).toContain("no longer exists");
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

describe("createLocalFailures: reconciliation by arm", () => {
  it("stands two DIFFERENT arms, in the order they were filed", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(staleBundle("b"));
    expect(armsOf(failures)).toEqual(["daemonUnreachable", "staleBundle"]);
  });

  it("REPLACES the entry when the same arm reports again", () => {
    // ARRANGE: a reconnect loop must not append a row per attempt.
    const failures = createLocalFailures();
    // ACT
    failures.report(daemonUnreachable(1006, "first"));
    failures.report(daemonUnreachable(1000, "second"));
    // ASSERT
    expect(failures.standing()).toHaveLength(1);
  });

  it("keeps the LATEST evidence after a replacement", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "first"));
    failures.report(daemonUnreachable(1000, "second"));
    expect(failures.standing()[0]?.evidence).toContainEqual(["close reason", "second"]);
  });

  it("keeps a repeat of one arm from disturbing another's place", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("standing"));
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(daemonUnreachable(1000, "b"));
    expect(armsOf(failures)).toEqual(["staleBundle", "daemonUnreachable"]);
  });
});

describe("createLocalFailures: retraction", () => {
  for (const [arm, mint] of MINTS) {
    it(`takes ${arm} down when it is retracted`, () => {
      const failures = createLocalFailures();
      failures.report(mint());
      failures.retract(arm);
      expect(failures.standing()).toEqual([]);
    });
  }

  it("leaves the other arms standing", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.report(staleBundle("b"));
    failures.retract("daemonUnreachable");
    expect(armsOf(failures)).toEqual(["staleBundle"]);
  });

  it("is a no-op for an arm that does not stand", () => {
    const failures = createLocalFailures();
    expect(() => failures.retract("daemonUnreachable")).not.toThrow();
  });

  it("lets workspaceGone stand, since nothing retracts it", () => {
    const failures = createLocalFailures();
    failures.report(workspaceGone());
    failures.retract("daemonUnreachable");
    expect(armsOf(failures)).toEqual(["workspaceGone"]);
  });

  it("re-files an arm after it was retracted", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.retract("daemonUnreachable");
    failures.report(daemonUnreachable(1006, "b"));
    expect(armsOf(failures)).toEqual(["daemonUnreachable"]);
  });
});

describe("createLocalFailures: subscribers", () => {
  it("tells a subscriber when an arm is filed", () => {
    const failures = createLocalFailures();
    const listener = vi.fn();
    failures.subscribe(listener);
    failures.report(staleBundle("x"));
    expect(listener).toHaveBeenCalledTimes(1);
  });

  it("tells a subscriber when an arm is retracted", () => {
    const failures = createLocalFailures();
    failures.report(staleBundle("x"));
    const listener = vi.fn();
    failures.subscribe(listener);
    failures.retract("staleBundle");
    expect(listener).toHaveBeenCalledTimes(1);
  });

  it("says nothing for a retraction that changed nothing", () => {
    const failures = createLocalFailures();
    const listener = vi.fn();
    failures.subscribe(listener);
    failures.retract("staleBundle");
    expect(listener).not.toHaveBeenCalled();
  });

  it("has the entry standing by the time the subscriber is told", () => {
    const failures = createLocalFailures();
    let seen: string[] = [];
    failures.subscribe(() => {
      seen = armsOf(failures);
    });
    failures.report(bootFailed("x"));
    expect(seen).toEqual(["bootFailed"]);
  });

  it("stops telling a subscriber once it unsubscribes", () => {
    const failures = createLocalFailures();
    const listener = vi.fn();
    failures.subscribe(listener)();
    failures.report(staleBundle("x"));
    expect(listener).not.toHaveBeenCalled();
  });
});

describe("createLocalFailures: refusals", () => {
  it("refuses a FailureKind that sets no arm", () => {
    const failures = createLocalFailures();
    expect(() => failures.report(create(FailureKindSchema, {}))).toThrow(MalformedView);
  });

  it("files nothing for a DAEMON-minted arm, which a frontend may not mint", () => {
    const failures = createLocalFailures();
    failures.report(
      create(FailureKindSchema, { kind: { case: "shimDegraded", value: { component: "stdout" } } }),
    );
    expect(failures.standing()).toEqual([]);
  });

  it("logs a foreign arm at error rather than swallowing it", async () => {
    const capture = captureLogRecords();
    const failures = createLocalFailures();
    failures.report(
      create(FailureKindSchema, { kind: { case: "shimDegraded", value: { component: "stdout" } } }),
    );
    expect((await forwardedRecord(capture, "warning-chip.foreign-arm")).level.case).toBe("error");
  });
});

describe("createLocalFailures: logging", () => {
  it("logs one record when a failure is filed", async () => {
    const capture = captureLogRecords();
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    const operations = await forwardedOperations(capture);
    expect(operations.filter((op) => op === "warning-chip.report")).toHaveLength(1);
  });

  it("logs a filed failure at error", async () => {
    const capture = captureLogRecords();
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    expect((await forwardedRecord(capture, "warning-chip.report")).level.case).toBe("error");
  });

  it("logs one record when a failure is cleared", async () => {
    const capture = captureLogRecords();
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    failures.retract("daemonUnreachable");
    const operations = await forwardedOperations(capture);
    expect(operations.filter((op) => op === "warning-chip.retract")).toHaveLength(1);
  });
});

describe("createLocalFailures: dispose", () => {
  it("forgets what stood", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    failures.dispose();
    expect(failures.standing()).toEqual([]);
  });

  it("tells subscribers the standing set emptied", () => {
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "a"));
    const listener = vi.fn();
    failures.subscribe(listener);
    failures.dispose();
    expect(listener).toHaveBeenCalledTimes(1);
  });
});

describe("createLocalFailures: suppress", () => {
  it("withholds the arm while the announced window stands", () => {
    // ARRANGE
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    // ACT
    failures.suppress("daemonUnreachable", NOW + 8000);
    failures.report(daemonUnreachable(1006, "gone"));
    // ASSERT
    expect(failures.standing()).toEqual([]);
  });

  it("still logs a report it withholds", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const capture = captureLogRecords();
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8000);
    failures.report(daemonUnreachable(1006, "gone"));
    expect(await forwardedOperations(capture)).toContain("warning-chip.suppressed");
  });

  it("files normally once the window has expired, since an overrun outage is news", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8000);
    vi.setSystemTime(NOW + 8001);
    failures.report(daemonUnreachable(1006, "gone"));
    expect(armsOf(failures)).toEqual(["daemonUnreachable"]);
  });

  it("takes down an entry already standing for the arm it starts suppressing", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.report(daemonUnreachable(1006, "gone"));
    failures.suppress("daemonUnreachable", NOW + 8000);
    expect(failures.standing()).toEqual([]);
  });

  it("suppresses only the arm it names", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8000);
    failures.report(bootFailed("no shell"));
    expect(armsOf(failures)).toEqual(["bootFailed"]);
  });

  it("clears the suppression on retract, so an early recovery is not muted on", () => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
    const failures = createLocalFailures();
    failures.suppress("daemonUnreachable", NOW + 8000);
    failures.retract("daemonUnreachable");
    failures.report(daemonUnreachable(1006, "gone"));
    expect(armsOf(failures)).toEqual(["daemonUnreachable"]);
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
