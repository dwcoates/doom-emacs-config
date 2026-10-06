/**
 * The keep-alive cadence and the yield it owes a real prompt.
 *
 * WHAT THIS GUARDS: that a user's prompt never builds on the harness's own
 * housekeeping. The failure mode being excluded is a real turn answered in a
 * context whose last several exchanges are keep-alives — the model reads them,
 * the user paid for them, and nothing on any surface says they are there.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  isKeepalivePrompt,
  KEEPALIVE_INTERVAL_MS,
  KEEPALIVE_PROMPT_MARKER,
  KeepaliveCadence,
  KeepaliveRewind,
  KeepaliveScope,
  keepalivePromptText,
  REAL_SCHEDULER,
  SPAN_UUIDS_PER_TURN,
  type KeepaliveAttribution,
} from "../../src/engine/keepalive.js";
import { SendLedger } from "../../src/engine/sends.js";
import { CACHE_TTL_1H_MS } from "../../src/engine/cold.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { ManualScheduler } from "./fakes.js";

/** An assistant record, the ONE shape the SDK says `resumeSessionAt` accepts. */
const assistant = (uuid: string): SdkMessage =>
  ({
    type: "assistant",
    uuid,
    session_id: "vendor-1",
    parent_tool_use_id: null,
    message: { role: "assistant", content: [] },
  }) as unknown as SdkMessage;

/** A vendor message of some other type that nonetheless carries a uuid. */
const other = (type: string, uuid: string, subtype?: string): SdkMessage =>
  ({ type, uuid, session_id: "vendor-1", ...(subtype === undefined ? {} : { subtype }) }) as unknown as SdkMessage;

/** The real turn a record arrived under. */
const realTurn = { turnId: "turn-1", keepalive: false } as const;
/** The shim's own keep-alive turn. */
const keepaliveTurn = { turnId: "keepalive-1", keepalive: true } as const;

describe("the keep-alive prompt", () => {
  it("BEGINS with the marker the store and sidecar classify on", () => {
    expect(keepalivePromptText(1).startsWith(KEEPALIVE_PROMPT_MARKER)).toBe(true);
  });

  it("recognizes its own prompts by that prefix, counter and all", () => {
    expect(isKeepalivePrompt(keepalivePromptText(7))).toBe(true);
  });

  it("does not mistake a user's prompt for one", () => {
    expect(isKeepalivePrompt("ship the feature")).toBe(false);
  });

  it("carries the counter it was given, so no two beats are identical", () => {
    // A repeated-identical prompt is the pattern that reinforced the echo loop
    // on 2026-09-17; the counter is what breaks it.
    expect(keepalivePromptText(1)).not.toBe(keepalivePromptText(2));
  });

  it("appends the counter after the instruction", () => {
    expect(keepalivePromptText(42).endsWith("(42)")).toBe(true);
  });
});

describe("the interval", () => {
  it("is fifty-two minutes", () => {
    expect(KEEPALIVE_INTERVAL_MS).toBe(52 * 60 * 1000);
  });

  it("beats inside subscription billing's one-hour cache window", () => {
    // A beat at or past the window would let the cache lapse between beats,
    // which is the exact cost the cadence exists to avoid paying. (The
    // 5-minute ephemeral tier is API billing's window, not subscription
    // billing's, which is why the interval is checked against the 1-hour one.)
    expect(KEEPALIVE_INTERVAL_MS).toBeLessThan(CACHE_TTL_1H_MS);
  });
});

describe("the yield obligation", () => {
  it("owes nothing when no keep-alive has run", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("uuid-1"), realTurn);

    expect(rewind.obligation()).toBeUndefined();
  });

  it("anchors on a GENUINE turn the vendor started on its own", () => {
    // Ruled 2026-10-06: a completed task's turn is real conversation, and a
    // rewind past it would lose it. (A stop's answer is tagged, never adopted.)
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(assistant("adopted-1"), { turnId: "adopted-1", keepalive: false, adopted: true });

    expect(rewind.anchorUuid()).toBe("adopted-1");
  });

  it("never anchors on the answer to a stop the keep-alive's rewind caused", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(assistant("stop-answer-1"), { turnId: "keepalive-1", keepalive: true, consequence: true });

    expect(rewind.anchorUuid()).toBe("real-1");
  });

  it("names the last REAL assistant record as the resume anchor", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("reports which turn the anchor came from", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.anchorTurnId).toBe("turn-1");
  });

  it("never anchors on a keep-alive's own assistant record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(assistant("keepalive-answer-1"), keepaliveTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on the `system:init` uuid", () => {
    // The uuid that killed the owner's session on 2026-09-14: required on the
    // init message, and naming no transcript record.
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("system", "19e047a0-init", "init"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on a `result` uuid", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("result", "b64f2741-result", "success"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("advances the anchor to a real turn's user record, an interrupted turn's tool result", () => {
    // Ruled 2026-10-06: the turn's own last record anchors, so the next rewind
    // keeps the result an interrupt left after the turn's last answer.
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(userRecord("tool-result-1"), realTurn);

    expect(rewind.anchorUuid()).toBe("tool-result-1");
  });

  it("advances the anchor to a genuine vendor-started turn's user record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(userRecord("vendor-result-1"), vendorTurn);

    expect(rewind.anchorUuid()).toBe("vendor-result-1");
  });

  it("never anchors on a keep-alive's own user record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(userRecord("keepalive-tool-result-1"), keepaliveTurn);

    expect(rewind.anchorUuid()).toBe("real-1");
  });

  it("never anchors on a stream event", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord(other("stream_event", "stream-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on a subagent's assistant record, which lives in the subagent's own transcript", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteRecord({ ...assistant("subagent-1"), parent_tool_use_id: "toolu_spawn" } as SdkMessage, realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.resumeSessionAt).toBe("real-1");
  });

  it("never anchors on an assistant record that belongs to no open turn", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("idle-1"), undefined);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("counts the keep-alive turns being discarded", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.discardedKeepaliveTurns).toBe(2);
  });

  it("owes nothing for keep-alive turns that PRECEDE the last real record", () => {
    // A keep-alive that ran before the session's first real prompt (nothing to
    // rewind TO, so that prompt carried it) lies BEHIND the real turn's anchor:
    // no rewind can discard it, and counting it made the next keep-alive
    // replace the vendor's query for nothing (2026-10-03, the e2e flake of
    // TestKeepAliveAnswerAfterVendorTurnNeverServed).
    const rewind = new KeepaliveRewind();
    rewind.noteKeepaliveTurn();
    rewind.noteRecord(assistant("real-1"), realTurn);

    expect(rewind.obligation()).toBeUndefined();
  });

  it("counts only the keep-alive turns AFTER the last real record", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteKeepaliveTurn();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.discardedKeepaliveTurns).toBe(1);
  });

  it("owes nothing when no real record exists to rewind TO", () => {
    // Truncating to nothing would discard the session's own opening.
    const rewind = new KeepaliveRewind();
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("owes nothing once the anchor is cleared at a boundary", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.clearAnchor("the vendor compacted the conversation");
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("reports no anchor at all once one is cleared", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.clearAnchor("the query was replaced without a rewind");

    expect(rewind.anchorUuid()).toBeUndefined();
  });

  it("clears the debt once the rewind is settled", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();
    rewind.settled();

    expect(rewind.obligation()).toBeUndefined();
  });

  it("KEEPS the anchor across a settled rewind, which does not cross a boundary", () => {
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteKeepaliveTurn();
    rewind.settled();

    expect(rewind.anchorUuid()).toBe("real-1");
  });
});

/** A `user` record (a tool result, a prompt) of the main conversation. */
const userRecord = (uuid: string): SdkMessage =>
  ({
    type: "user",
    uuid,
    session_id: "vendor-1",
    parent_tool_use_id: null,
    message: { role: "user", content: [] },
  }) as unknown as SdkMessage;

/** A vendor turn the keep-alive's own rewind set off. */
const consequenceTurn = { turnId: "keepalive-1", keepalive: true, consequence: true } as const;
/** A genuine turn the vendor started on its own. */
const vendorTurn = { turnId: "adopted-1", keepalive: false, adopted: true } as const;

/** A rewind anchored on a real turn, with one whole keep-alive turn after it. */
function anchoredWithKeepalive(): KeepaliveRewind {
  const rewind = new KeepaliveRewind();
  rewind.noteRecord(assistant("real-1"), realTurn);
  rewind.noteSend("keepalive-send-1", keepaliveTurn);
  rewind.noteRecord(assistant("keepalive-answer-1"), keepaliveTurn);
  rewind.noteKeepaliveTurn();
  return rewind;
}

/**
 * THE SPAN INVARIANT: a rewind discards only keep-alive material.
 *
 * Everything between the anchor and now must be the keep-alive's own turn or
 * a vendor turn its rewind set off; anything else refuses the rewind, at
 * ERROR, with the content kept.
 */
describe("the span invariant", () => {
  it("lets a rewind discard the keep-alive's own send and answer", () => {
    const rewind = anchoredWithKeepalive();

    expect(rewind.obligation()?.span).toEqual([
      { turnId: "keepalive-1", kind: "keepalive", uuids: ["keepalive-send-1", "keepalive-answer-1"], records: 2 },
    ]);
  });

  it("lets one rewind discard several keep-alives in a row", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("keepalive-send-2", { turnId: "keepalive-2", keepalive: true });
    rewind.noteKeepaliveTurn();

    expect(rewind.obligation()?.span.map((turn) => turn.turnId)).toEqual(["keepalive-1", "keepalive-2"]);
  });

  it("lets a rewind discard a vendor turn the keep-alive's rewind set off", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteRecord(assistant("stop-answer-1"), consequenceTurn);

    expect(rewind.obligation()?.span.map((turn) => turn.kind)).toEqual(["keepalive", "keepalive_consequence"]);
  });

  it("refuses a rewind whose span holds a real prompt", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });

    expect(rewind.obligation()).toBeUndefined();
  });

  it("refuses a rewind whose span holds a record no turn owns", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteRecord(assistant("orphan-1"), undefined);

    expect(rewind.obligation()).toBeUndefined();
  });

  it("records a refusal at ERROR, naming each offending turn, its kind and its records", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });
    const mark = logSinkMark();

    rewind.obligation();

    const record = logRecordsSince(mark).find((entry) => entry.message.startsWith("the keep-alive rewind is REFUSED"));
    expect({ level: record?.level, offending: record?.context.offending }).toEqual({
      level: "error",
      offending: [{ turn_id: "turn-2", kind: "real", records: 1, uuids: ["real-send-2"] }],
    });
  });

  it("states the offending turns in the refusal's detail text", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });
    const mark = logSinkMark();

    rewind.obligation();

    const record = logRecordsSince(mark).find((entry) => entry.message.startsWith("the keep-alive rewind is REFUSED"));
    expect(record?.context.detail).toBe("real turn turn-2: real-send-2");
  });

  it("names the anchor the refused rewind would have resumed at", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });
    const mark = logSinkMark();

    rewind.obligation();

    const record = logRecordsSince(mark).find((entry) => entry.message.startsWith("the keep-alive rewind is REFUSED"));
    expect(record?.context.resume_session_at).toBe("real-1");
  });

  it("drops the anchor on a refusal, so the content stays for good", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });

    rewind.obligation();

    expect(rewind.anchorUuid()).toBeUndefined();
  });

  it("settles the debt on a refusal, so it is refused once, not every beat", () => {
    const rewind = anchoredWithKeepalive();
    rewind.noteSend("real-send-2", { turnId: "turn-2", keepalive: false });

    rewind.obligation();

    expect(rewind.debt()).toBe(0);
  });

  it("keeps an outstanding keep-alive's send in the span across a vendor turn that anchored ahead of it", () => {
    // The vendor ran a completed task's turn before answering the keep-alive's
    // send: the anchor lies before the keep-alive's prompt record.
    const rewind = new KeepaliveRewind();
    rewind.noteRecord(assistant("real-1"), realTurn);
    rewind.noteSend("keepalive-send-1", keepaliveTurn);

    rewind.noteRecord(assistant("vendor-answer-1"), vendorTurn);

    expect(rewind.span()).toEqual([
      { turnId: "keepalive-1", kind: "keepalive", uuids: ["keepalive-send-1"], records: 1 },
    ]);
  });

  it("forgets the keep-alive's send once its turn ended", () => {
    const rewind = anchoredWithKeepalive();

    rewind.noteRecord(assistant("vendor-answer-1"), vendorTurn);

    expect(rewind.span()).toEqual([]);
  });

  it("empties the span on a new anchor", () => {
    const rewind = anchoredWithKeepalive();

    rewind.noteRecord(assistant("real-2"), { turnId: "turn-2", keepalive: false });

    expect(rewind.span()).toEqual([]);
  });

  it("empties the span once a rewind is settled", () => {
    const rewind = anchoredWithKeepalive();

    rewind.settled();

    expect(rewind.span()).toEqual([]);
  });

  it("empties the span when the anchor is cleared at a boundary", () => {
    const rewind = anchoredWithKeepalive();

    rewind.clearAnchor("the vendor compacted the conversation");

    expect(rewind.span()).toEqual([]);
  });

  it("tracks nothing while no anchor stands", () => {
    const rewind = new KeepaliveRewind();

    rewind.noteSend("keepalive-send-1", keepaliveTurn);

    expect(rewind.span()).toEqual([]);
  });

  it("keeps a subagent's records out of the span", () => {
    const rewind = anchoredWithKeepalive();

    rewind.noteRecord({ ...assistant("subagent-1"), parent_tool_use_id: "toolu_spawn" } as SdkMessage, vendorTurn);

    expect(rewind.obligation()?.span.map((turn) => turn.kind)).toEqual(["keepalive"]);
  });

  it("keeps a `result` uuid out of the span", () => {
    const rewind = anchoredWithKeepalive();

    rewind.noteRecord(other("result", "vendor-result-1", "success"), vendorTurn);

    expect(rewind.obligation()?.span.map((turn) => turn.kind)).toEqual(["keepalive"]);
  });

  it("lists a re-delivered send's uuid once", () => {
    const rewind = anchoredWithKeepalive();

    rewind.noteSend("keepalive-send-1", keepaliveTurn);

    expect(rewind.span()[0]?.uuids).toEqual(["keepalive-send-1", "keepalive-answer-1"]);
  });

  it("caps the uuids a span turn lists while its record count stays exact", () => {
    const rewind = anchoredWithKeepalive();

    for (let index = 0; index < SPAN_UUIDS_PER_TURN; index++) {
      rewind.noteRecord(assistant(`keepalive-block-${index}`), keepaliveTurn);
    }

    expect([rewind.span()[0]?.uuids.length, rewind.span()[0]?.records]).toEqual([
      SPAN_UUIDS_PER_TURN,
      SPAN_UUIDS_PER_TURN + 2,
    ]);
  });

  it("states the keep-alive debt for a caller that must assert none is owed", () => {
    const rewind = anchoredWithKeepalive();

    expect(rewind.debt()).toBe(1);
  });
});

describe("the cadence", () => {
  it("beats when it is running", () => {
    const scheduler = new ManualScheduler();
    let beats = 0;
    new KeepaliveCadence(() => beats++, 1, scheduler).start();

    scheduler.fire();

    expect(beats).toBe(1);
  });

  it("does not double the cadence on a second start", () => {
    const scheduler = new ManualScheduler();
    const cadence = new KeepaliveCadence(() => undefined, 1, scheduler);

    cadence.start();
    cadence.start();

    expect(scheduler.handlers).toHaveLength(1);
  });

  it("SKIPS the beat while a turn is in flight", () => {
    // A keep-alive submitted into an open turn would be a second submitter.
    const scheduler = new ManualScheduler();
    let beats = 0;
    const cadence = new KeepaliveCadence(() => beats++, 1, scheduler);
    cadence.start();
    cadence.pause();

    scheduler.fire();

    expect(beats).toBe(0);
  });

  it("beats again once the turn ends", () => {
    const scheduler = new ManualScheduler();
    let beats = 0;
    const cadence = new KeepaliveCadence(() => beats++, 1, scheduler);
    cadence.start();
    cadence.pause();
    cadence.resume();

    scheduler.fire();

    expect(beats).toBe(1);
  });

  it("clears its interval when stopped", () => {
    const scheduler = new ManualScheduler();
    const cadence = new KeepaliveCadence(() => undefined, 1, scheduler);
    cadence.start();

    cadence.stop();

    expect(scheduler.cleared).toBe(1);
  });

  it("is a no-op to stop one that never started", () => {
    const scheduler = new ManualScheduler();

    new KeepaliveCadence(() => undefined, 1, scheduler).stop();

    expect(scheduler.cleared).toBe(0);
  });
});

/**
 * REAL_SCHEDULER is the cadence's default (every unit test above injects
 * ManualScheduler instead), wired in production whenever `createEngine` is
 * not handed a scheduler override. Pin the real setInterval/clearInterval
 * wiring directly rather than only through the fake.
 */
describe("REAL_SCHEDULER", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("setInterval beats the handler on the given cadence", () => {
    const beats: number[] = [];
    REAL_SCHEDULER.setInterval(() => beats.push(1), 10);

    vi.advanceTimersByTime(35);

    expect(beats.length).toBe(3);
  });

  it("clearInterval stops further beats", () => {
    const beats: number[] = [];
    const handle = REAL_SCHEDULER.setInterval(() => beats.push(1), 10);

    vi.advanceTimersByTime(15);
    REAL_SCHEDULER.clearInterval(handle);
    vi.advanceTimersByTime(100);

    expect(beats.length).toBe(1);
  });
});

/** The client uuid the keep-alive send carries in these tests. */
const KEEPALIVE_SEND = "00000000-0000-4000-8000-00000000ka01";
/** Some OTHER send's client uuid. */
const OTHER_SEND = "00000000-0000-4000-8000-00000000re01";

/** The echo a reply frame carries: the complete list and the single field. */
const stampedWith = (send: string): Record<string, unknown> => ({
  user_message_uuid: send,
  user_message_uuids: [send],
});

/** A top-level assistant frame, optionally naming the send it answers. */
const reply = (stamp: Record<string, unknown> = {}): SdkMessage =>
  ({ ...assistant("assistant-uuid"), ...stamp });

/** A top-level stream event, optionally naming the send it answers. */
const streamEvent = (stamp: Record<string, unknown> = {}): SdkMessage =>
  ({
    type: "stream_event",
    uuid: "stream-uuid",
    session_id: "vendor-1",
    parent_tool_use_id: null,
    event: { type: "message_start" },
    ...stamp,
  }) as unknown as SdkMessage;

/** A thinking-progress frame, optionally naming the send it answers. */
const thinkingTokens = (stamp: Record<string, unknown> = {}): SdkMessage =>
  ({
    type: "system",
    subtype: "thinking_tokens",
    estimated_tokens: 10,
    estimated_tokens_delta: 10,
    uuid: "thinking-uuid",
    session_id: "vendor-1",
    ...stamp,
  }) as unknown as SdkMessage;

/** A turn's result, optionally naming the send it answers. */
const result = (stamp: Record<string, unknown> = {}): SdkMessage =>
  ({ ...other("result", "result-uuid", "success"), ...stamp });

/** A subagent's frame inside whatever turn is running: never stamped. */
const subagentReply = (parent = "toolu_spawn", opens: readonly string[] = []): SdkMessage =>
  ({
    ...assistant("subagent-uuid"),
    parent_tool_use_id: parent,
    message: { role: "assistant", content: opens.map((id) => ({ type: "tool_use", id, name: "Agent", input: {} })) },
  }) as unknown as SdkMessage;

/** A top-level reply that opens `toolu_spawn`, optionally naming the send it answers. */
const spawningReply = (stamp: Record<string, unknown> = {}): SdkMessage =>
  ({
    ...reply(stamp),
    message: { role: "assistant", content: [{ type: "tool_use", id: "toolu_spawn", name: "Agent", input: {} }] },
  }) as unknown as SdkMessage;

/** A vendor task's lifecycle message, naming the call that opened it when `toolUseId` is given. */
const taskMessage = (subtype: string, taskId: string, toolUseId?: string): SdkMessage =>
  ({
    type: "system",
    subtype,
    task_id: taskId,
    ...(toolUseId === undefined ? {} : { tool_use_id: toolUseId }),
    uuid: `${subtype}-uuid`,
    session_id: "vendor-1",
  }) as unknown as SdkMessage;

/**
 * The keep-alive scope AS THE SESSION DRIVES IT: beside the one send ledger
 * that reads every echo (engine/sends.ts). Its keep-alive send is registered in
 * the ledger the way the session registers every send, and each message is
 * attributed by the ledger first and tagged by the scope from that verdict.
 */
class LedgeredScope {
  readonly ledger = new SendLedger();
  readonly scope = new KeepaliveScope();

  begin(uuid: string, turnId: string): void {
    this.scope.begin(uuid, turnId);
    this.ledger.sent({ uuid, turnId, keepalive: true });
  }

  attribute(message: SdkMessage): KeepaliveAttribution {
    return this.scope.attribute(message, this.ledger.attribute(message));
  }

  abandon(reason: string): void {
    const held = this.scope.pendingUuid();
    this.scope.abandon(reason);
    if (held !== undefined) this.ledger.forget("keepalive-1", reason);
  }

  queryBound(): void {
    this.scope.queryBound();
    this.ledger.queryBound();
  }

  pendingUuid(): string | undefined {
    return this.scope.pendingUuid();
  }

  /** A real send pushed onto the query, as the session registers it. */
  sent(uuid: string, turnId: string): void {
    this.ledger.sent({ uuid, turnId, keepalive: false });
  }

  rewound(): void {
    this.queryBound();
    this.scope.rewound();
  }

  producing(): boolean {
    return this.scope.producing();
  }

  spawned(toolUseId: string): boolean {
    return this.scope.spawned(toolUseId);
  }
}

/** A scope with no keep-alive pending. */
function idleScope(): LedgeredScope {
  return new LedgeredScope();
}

/** A scope with the keep-alive send pending. */
function pendingScope(): LedgeredScope {
  const scope = new LedgeredScope();
  scope.begin(KEEPALIVE_SEND, "keepalive-1");
  return scope;
}

/**
 * THE KEEP-ALIVE TURN SCOPE.
 *
 * WHAT THIS GUARDS: that the tag every row and push of a vendor message carries
 * comes from the SEND the vendor says the message answers, never from what was
 * open when it arrived. The failure mode being excluded is the leak of
 * 2026-09-23: a turn the vendor ran on its own closed the keep-alive, and the
 * keep-alive's own answer then arrived untagged and was served.
 */
describe("the keep-alive turn scope", () => {
  it("tags nothing while no keep-alive is pending", () => {
    // Arrange
    const scope = idleScope();

    // Act
    const tagged = scope.attribute(reply(stampedWith(OTHER_SEND)));

    // Assert
    expect(tagged).toEqual({ keepalive: false, endsKeepalive: false });
  });

  it.each([
    ["a top-level assistant message", reply],
    ["a top-level stream event", streamEvent],
    ["a thinking-progress frame", thinkingTokens],
  ])("tags %s that names the keep-alive's send", (_name, frame) => {
    // Arrange
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(frame(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(tagged).toEqual({ keepalive: true, endsKeepalive: false });
  });

  it("tags a frame whose batch list names the keep-alive among other sends", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(
      reply({ user_message_uuid: OTHER_SEND, user_message_uuids: [KEEPALIVE_SEND, OTHER_SEND] }),
    );

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("falls back to the single field when the producer states no list", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(reply({ user_message_uuid: KEEPALIVE_SEND }));

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("carries the tag to the keep-alive turn's later, unstamped frames", () => {
    // Arrange: the vendor stamps only the FIRST reply of each kind.
    const scope = pendingScope();
    scope.attribute(streamEvent(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("carries the tag to the frame of a subagent the keep-alive spawned", () => {
    // Arrange: the subagent is the keep-alive's only because its reply opened it.
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(subagentReply());

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("does not tag a backgrounded subagent's frame arriving during the keep-alive's turn", () => {
    // Arrange: THE 2026-09-15 SEQUENCE — a real turn launched the subagent in
    // the background, and its frames keep arriving while the keep-alive runs.
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(subagentReply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("carries the tag to a subagent the keep-alive's own subagent spawned", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));
    scope.attribute(subagentReply("toolu_spawn", ["toolu_nested"]));

    // Act
    const tagged = scope.attribute(subagentReply("toolu_nested"));

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("does not tag a task notification for work the keep-alive did not start", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(taskMessage("task_notification", "task-bg", "toolu_background"));

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("tags the start of a task the keep-alive's own call opened", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(taskMessage("task_started", "task-ka", "toolu_spawn"));

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("tags an update of a task the keep-alive started", () => {
    // Arrange: an update names only the task, which the start bound to the call.
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));
    scope.attribute(taskMessage("task_started", "task-ka", "toolu_spawn"));

    // Act
    const tagged = scope.attribute(taskMessage("task_updated", "task-ka"));

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("does not tag an update of a task the keep-alive did not start", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(taskMessage("task_updated", "task-bg"));

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("answers spawned() for a call the keep-alive's turn opened", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(scope.spawned("toolu_spawn")).toBe(true);
  });

  it("does not remember a call a turn it did not tag opened", () => {
    // Arrange
    const scope = pendingScope();

    // Act: a real turn's reply opens the call.
    scope.attribute(spawningReply(stampedWith(OTHER_SEND)));

    // Assert
    expect(scope.spawned("toolu_spawn")).toBe(false);
  });

  it("forgets its spawns once its own result closes the scope", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));

    // Act
    scope.attribute(result(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(scope.spawned("toolu_spawn")).toBe(false);
  });

  it("forgets its spawns when abandoned", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(spawningReply(stampedWith(KEEPALIVE_SEND)));

    // Act
    scope.abandon("the vendor query died");

    // Assert
    expect(scope.spawned("toolu_spawn")).toBe(false);
  });

  it("ignores a stamp on a subagent frame", () => {
    // Arrange: a subagent frame is never the one that names a turn's send.
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute({ ...subagentReply(), ...stampedWith(KEEPALIVE_SEND) });

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("closes the scope on the keep-alive's own result", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    const tagged = scope.attribute(result(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect([tagged, scope.pendingUuid()]).toEqual([{ keepalive: true, endsKeepalive: true }, undefined]);
  });

  it("closes the scope on a stamped result that follows no reply at all", () => {
    // Arrange: a turn a hook refused answers with its result alone.
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(result(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(tagged).toEqual({ keepalive: true, endsKeepalive: true });
  });

  it("does not tag a turn whose first reply names no send", () => {
    // Arrange: the vendor's own task-notification turn, while the keep-alive waits.
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("tags the turn the vendor runs to answer a stop the keep-alive's rewind caused", () => {
    // Arrange: the rewind replaced the query; the vendor reports the old
    // query's background shell as stopped, then answers that in a turn.
    const scope = pendingScope();
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("ends that turn as the keep-alive's without ending the keep-alive", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);
    scope.attribute(reply());

    // Act
    const tagged = scope.attribute(result());

    // Assert
    expect([tagged, scope.pendingUuid()]).toEqual([{ keepalive: true, endsKeepalive: false }, KEEPALIVE_SEND]);
  });

  it("serves a turn answering a genuine completion beside the keep-alive", () => {
    // Arrange: a completed background task, not a stop.
    const scope = pendingScope();
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "completed" } as SdkMessage);

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("tags the turn answering a stop a REAL prompt's rewind caused, with no keep-alive pending", () => {
    // Arrange: the rewind ran for a real prompt; the stop is reported ahead
    // of that prompt's own turn (the ship-gns replay).
    const scope = idleScope();
    scope.rewound();
    scope.sent("real-send-1", "turn-1");
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("stops watching for the rewind's stop once the send's own turn began", () => {
    // Arrange
    const scope = idleScope();
    scope.rewound();
    scope.sent("real-send-1", "turn-1");
    scope.attribute(reply(stampedWith("real-send-1")));
    scope.attribute(result(stampedWith("real-send-1")));
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("serves a stop's answer after a query binding that was no rewind", () => {
    // Arrange: a user's own stop of a task, on an ordinary query.
    const scope = idleScope();
    scope.queryBound();
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("claims no later vendor turn once the stop's turn ended", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute({ ...other("system", "notif-1", "task_notification"), task_id: "t-1", status: "stopped" } as SdkMessage);
    scope.attribute(reply());
    scope.attribute(result());

    // Act
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("keeps the keep-alive pending across a vendor turn's unstamped result", () => {
    // Arrange: THE 2026-09-23 SEQUENCE — the vendor's own turn ends first.
    const scope = pendingScope();
    scope.attribute(reply());

    // Act
    const tagged = scope.attribute(result());

    // Assert
    expect([tagged, scope.pendingUuid()]).toEqual([{ keepalive: false, endsKeepalive: false }, KEEPALIVE_SEND]);
  });

  it("keeps the keep-alive pending across a vendor turn with no reply at all", () => {
    // Arrange: the observed vendor turn was an `init` and a `result`, nothing else.
    const scope = pendingScope();
    scope.attribute(other("system", "init-uuid", "init"));

    // Act
    const tagged = scope.attribute(result());

    // Assert
    expect([tagged.keepalive, scope.pendingUuid()]).toEqual([false, KEEPALIVE_SEND]);
  });

  it("recognizes the keep-alive's answer after a vendor turn ran first", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply());
    scope.attribute(result());

    // Act
    const tagged = scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(tagged.keepalive).toBe(true);
  });

  it("leaves a vendor turn's frames untagged before it folds the keep-alive in", () => {
    // Arrange: a running meta turn takes the queued keep-alive in mid-turn.
    const scope = pendingScope();

    // Act
    const before = scope.attribute(reply());

    // Assert
    expect(before.keepalive).toBe(false);
  });

  it("tags a vendor turn's frames from the moment it folds the keep-alive in", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply());

    // Act
    const after = scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(after.keepalive).toBe(true);
  });

  it("does not tag a turn whose first stream event names no send", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(streamEvent());

    // Act: the same turn's later blocks carry no stamp either.
    const tagged = scope.attribute(reply());

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("does not tag a frame that names some other send", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(reply(stampedWith(OTHER_SEND)));

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("leaves a turn's unstamped preamble untagged", () => {
    // Arrange: nothing before the first reply says whose turn it is.
    const scope = pendingScope();

    // Act
    const tagged = scope.attribute(other("system", "init-uuid", "init"));

    // Assert
    expect(tagged.keepalive).toBe(false);
  });

  it("forgets the running turn when a new query is bound", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    scope.queryBound();

    // Assert
    expect([scope.producing(), scope.pendingUuid()]).toEqual([false, KEEPALIVE_SEND]);
  });

  it("answers producing() while the keep-alive's turn runs", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Assert
    expect(scope.producing()).toBe(true);
  });

  it("REFUSES a second keep-alive while one is unanswered", () => {
    // Arrange
    const scope = pendingScope();

    // Act
    const second = (): void => scope.begin("another-send", "keepalive-2");

    // Assert
    expect(second).toThrow(/keepalive-2 began while keepalive-1 was still unanswered/);
  });

  it("closes the scope without an answer when abandoned", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply(stampedWith(KEEPALIVE_SEND)));

    // Act
    scope.abandon("the vendor query died");

    // Assert
    expect([scope.pendingUuid(), scope.producing()]).toEqual([undefined, false]);
  });

  it("records an abandoned keep-alive at INFO through the canonical logger", () => {
    // Arrange
    const scope = pendingScope();
    const before = logSinkMark();

    // Act
    scope.abandon("the vendor query died");

    // Assert
    expect(
      logRecordsSince(before).map((record) => [record.level, record.message, record.context.reason]),
    ).toContainEqual(["info", "a keep-alive's turn scope closed WITHOUT its answer", "the vendor query died"]);
  });

  it("records nothing when abandoning with no keep-alive pending", () => {
    // Arrange
    const scope = idleScope();
    const before = logSinkMark();

    // Act
    scope.abandon("nothing was pending");

    // Assert
    expect(logRecordsSince(before)).toEqual([]);
  });

  it("records a vendor turn ending under a waiting keep-alive at INFO", () => {
    // Arrange
    const scope = pendingScope();
    scope.attribute(reply());
    const before = logSinkMark();

    // Act
    scope.attribute(result());

    // Assert
    expect(logRecordsSince(before).map((record) => [record.level, record.message])).toContainEqual([
      "info",
      "a vendor turn the keep-alive did not start ended; the keep-alive's own turn stays open",
    ]);
  });
});
