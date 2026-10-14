import { describe, expect, it } from "vitest";
import { ForkPointTracker, type OpenForkTurn } from "../../src/engine/fork-point.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

const NOW = 9_000;

/** An open turn as GetForkPoint hands it to the tracker. */
function openTurn(turnId: string, startedAtMs: number, openerUuid: string | undefined): OpenForkTurn {
  return { turnId, startedAtMs, openerUuid };
}

describe("ForkPointTracker.point", () => {
  it.each([
    {
      name: "an idle shim that concluded nothing forks the whole transcript, bounded at now",
      arrange: (_t: ForkPointTracker): void => undefined,
      open: [] as OpenForkTurn[],
      expected: { kind: "whole", throughMs: NOW, uninherited: [] },
    },
    {
      name: "a concluded turn's last record is the point, bounded at its conclusion",
      arrange: (t: ForkPointTracker): void => {
        t.noteRecord("t1", "prompt-1");
        t.noteRecord("t1", "reply-1");
        t.noteConcluded("t1", 100);
      },
      open: [] as OpenForkTurn[],
      expected: { kind: "through_record", recordUuid: "reply-1", throughMs: 100, uninherited: [] },
    },
    {
      name: "the newest conclusion wins over an earlier one",
      arrange: (t: ForkPointTracker): void => {
        t.noteRecord("t1", "reply-1");
        t.noteConcluded("t1", 100);
        t.noteRecord("t2", "reply-2");
        t.noteConcluded("t2", 200);
      },
      open: [] as OpenForkTurn[],
      expected: { kind: "through_record", recordUuid: "reply-2", throughMs: 200, uninherited: [] },
    },
    {
      name: "a turn in flight is uninherited, and the point stays on the concluded turn",
      arrange: (t: ForkPointTracker): void => {
        t.noteRecord("t1", "reply-1");
        t.noteConcluded("t1", 100);
        t.noteRecord("t2", "prompt-2");
      },
      open: [openTurn("t2", 150, "prompt-2")],
      expected: { kind: "through_record", recordUuid: "reply-1", throughMs: 100, uninherited: ["t2"] },
    },
    {
      name: "a turn concluded with no record keeps the earlier record and moves the bound",
      arrange: (t: ForkPointTracker): void => {
        t.noteRecord("t1", "reply-1");
        t.noteConcluded("t1", 100);
        t.noteConcluded("t2", 300);
      },
      open: [] as OpenForkTurn[],
      expected: { kind: "through_record", recordUuid: "reply-1", throughMs: 300, uninherited: [] },
    },
    {
      name: "with nothing concluded, a turn in flight cuts before its opener, bounded at its start",
      arrange: (_t: ForkPointTracker): void => undefined,
      open: [openTurn("t2", 150, "prompt-2")],
      expected: { kind: "before_record", recordUuid: "prompt-2", throughMs: 150, uninherited: ["t2"] },
    },
    {
      name: "with two turns open, the cut is before the earlier one's opener",
      arrange: (_t: ForkPointTracker): void => undefined,
      open: [openTurn("t3", 250, "prompt-3"), openTurn("t2", 150, "prompt-2")],
      expected: { kind: "before_record", recordUuid: "prompt-2", throughMs: 150, uninherited: ["t3", "t2"] },
    },
  ])("$name", ({ arrange, open, expected }) => {
    // Arrange
    const tracker = new ForkPointTracker();
    arrange(tracker);

    // Act
    const answer = tracker.point(open, NOW);

    // Assert
    expect(answer).toEqual(expected);
  });

  it("is unresolved when nothing concluded and the turn in flight has no known opener", () => {
    // Arrange
    const tracker = new ForkPointTracker();

    // Act
    const answer = tracker.point([openTurn("t2", 150, undefined)], NOW);

    // Assert
    expect(answer).toMatchObject({ kind: "unresolved" });
    expect(answer.kind === "unresolved" ? answer.detail : "").toContain("t2");
  });
});

describe("ForkPointTracker bookkeeping", () => {
  it("answers a turn's first record and forgets it once the turn concludes", () => {
    // Arrange
    const tracker = new ForkPointTracker();
    tracker.noteRecord("t1", "first");
    tracker.noteRecord("t1", "second");

    // Act
    const before = tracker.firstRecord("t1");
    tracker.noteConcluded("t1", 100);

    // Assert
    expect(before).toBe("first");
    expect(tracker.firstRecord("t1")).toBeUndefined();
  });

  it("ignores a record with no turn or no uuid", () => {
    // Arrange
    const tracker = new ForkPointTracker();

    // Act
    tracker.noteRecord("", "orphan");
    tracker.noteRecord("t1", "");

    // Assert
    expect(tracker.firstRecord("")).toBeUndefined();
    expect(tracker.firstRecord("t1")).toBeUndefined();
  });

  it("a reset forgets the point and every open turn's records, and says so", () => {
    // Arrange
    const tracker = new ForkPointTracker();
    tracker.noteRecord("t1", "reply-1");
    tracker.noteConcluded("t1", 100);
    tracker.noteRecord("t2", "prompt-2");
    const mark = logSinkMark();

    // Act
    tracker.reset("rotated");

    // Assert
    expect(tracker.point([], NOW)).toEqual({ kind: "whole", throughMs: NOW, uninherited: [] });
    expect(tracker.firstRecord("t2")).toBeUndefined();
    expect(logRecordsSince(mark)).toMatchObject([
      { level: "info", context: { reason: "rotated", forgotten_point: "reply-1", open_turns: 1 } },
    ]);
  });

  it("a reset of a tracker holding nothing logs nothing", () => {
    // Arrange
    const tracker = new ForkPointTracker();
    const mark = logSinkMark();

    // Act
    tracker.reset("rotated");

    // Assert
    expect(logRecordsSince(mark)).toEqual([]);
  });
});
