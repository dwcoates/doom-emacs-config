/**
 * The send ledger: which of the shim's sends each vendor turn answers.
 *
 * WHAT THIS GUARDS: the owner's ruling of 2026-09-28, that a reply is matched
 * to the send that caused it BY ITS ECHO, never by the order frames arrive in.
 * The failure mode being excluded is a vendor-started turn (a hand-back, a task
 * notification) landing the instant before a send's own answer and being
 * charged to that send.
 */
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  OPEN_SENDS_BOUND,
  RETIRED_SENDS_REMEMBERED,
  SendLedger,
  isTopLevelReply,
  stampedSends,
  type Send,
  type SendVerdict,
} from "../../src/engine/sends.js";
import type { SdkMessage } from "../../src/sdk/types.js";

const FIRST: Send = { uuid: "00000000-0000-4000-8000-000000000001", turnId: "turn-1", keepalive: false };
const SECOND: Send = { uuid: "00000000-0000-4000-8000-000000000002", turnId: "turn-2", keepalive: false };
const KEEPALIVE: Send = { uuid: "00000000-0000-4000-8000-0000000000ka", turnId: "keepalive-1", keepalive: true };
const STRANGER = "00000000-0000-4000-8000-00000000dead";

/** The echo a reply frame carries: the complete list and the single field. */
const stamp = (...uuids: string[]): Record<string, unknown> => ({
  user_message_uuid: uuids.at(-1),
  user_message_uuids: uuids,
});

const reply = (echo: Record<string, unknown> = {}): SdkMessage =>
  ({
    type: "assistant",
    uuid: "assistant-uuid",
    session_id: "vendor-1",
    parent_tool_use_id: null,
    message: { role: "assistant", content: [] },
    ...echo,
  }) as unknown as SdkMessage;

const subagentReply = (): SdkMessage => ({ ...reply(), parent_tool_use_id: "toolu_spawn" }) as unknown as SdkMessage;

const streamEvent = (echo: Record<string, unknown> = {}): SdkMessage =>
  ({
    type: "stream_event",
    uuid: "stream-uuid",
    session_id: "vendor-1",
    parent_tool_use_id: null,
    event: { type: "message_start" },
    ...echo,
  }) as unknown as SdkMessage;

const thinking = (echo: Record<string, unknown> = {}): SdkMessage =>
  ({ type: "system", subtype: "thinking_tokens", uuid: "thinking-uuid", session_id: "vendor-1", ...echo }) as unknown as SdkMessage;

const result = (echo: Record<string, unknown> = {}): SdkMessage =>
  ({ type: "result", subtype: "success", uuid: "result-uuid", session_id: "vendor-1", ...echo }) as unknown as SdkMessage;

const hook = (): SdkMessage =>
  ({ type: "system", subtype: "hook_response", uuid: "hook-uuid", session_id: "vendor-1" }) as unknown as SdkMessage;

/** A ledger with the given sends open. */
function ledgerWith(...sends: Send[]): SendLedger {
  const ledger = new SendLedger();
  for (const send of sends) ledger.sent(send);
  return ledger;
}

/** The verdict's turn, rendered for a table's expectation. */
function turnOf(verdict: SendVerdict): string {
  const turn = verdict.turn;
  return turn.kind === "send" ? `send:${turn.send.turnId}${turn.retired ? ":retired" : ""}` : turn.kind;
}

describe("stampedSends", () => {
  it.each([
    ["an assistant reply's single field", reply({ user_message_uuid: FIRST.uuid }), FIRST.uuid],
    ["an assistant reply's list, read as its last member", reply({ user_message_uuids: [FIRST.uuid, SECOND.uuid] }), SECOND.uuid],
    ["a stream event", streamEvent(stamp(FIRST.uuid)), FIRST.uuid],
    ["a thinking-tokens frame", thinking({ user_message_uuid: FIRST.uuid }), FIRST.uuid],
    ["a result", result(stamp(FIRST.uuid)), FIRST.uuid],
  ])("reads the send %s answers", (_name, message, answers) => {
    // Act
    const stamps = stampedSends(message);

    // Assert
    expect(stamps?.answers).toBe(answers);
  });

  it.each([
    ["a subagent's frame, which the vendor never stamps", { ...subagentReply(), ...stamp(FIRST.uuid) }],
    ["an unstamped reply", reply()],
    ["a hook, which is no reply", hook()],
    ["an empty single field", reply({ user_message_uuid: "" })],
  ])("reads no send from %s", (_name, message) => {
    // Act
    const stamps = stampedSends(message);

    // Assert
    expect(stamps).toBeUndefined();
  });

  it("keeps the single field in the complete list even when the list omits it", () => {
    // Act
    const stamps = stampedSends(reply({ user_message_uuid: SECOND.uuid, user_message_uuids: [FIRST.uuid] }));

    // Assert
    expect(stamps?.all).toEqual([FIRST.uuid, SECOND.uuid]);
  });
});

describe("isTopLevelReply", () => {
  it.each([
    ["a top-level assistant message", reply(), true],
    ["a top-level stream event", streamEvent(), true],
    ["a thinking-tokens frame", thinking(), true],
    ["a subagent's assistant message", subagentReply(), false],
    ["a hook", hook(), false],
    ["a result", result(), false],
  ])("answers %s", (_name, message, expected) => {
    // Act / Assert
    expect(isTopLevelReply(message)).toBe(expected);
  });
});

describe("the send ledger", () => {
  it.each<[string, Send[], SdkMessage[], string]>([
    ["a reply naming an open send is that send's", [FIRST], [reply(stamp(FIRST.uuid))], "send:turn-1"],
    ["a reply naming nothing opens a vendor-started turn", [FIRST], [reply()], "vendor"],
    ["a result naming nothing, with no reply ahead, is a vendor-started turn", [FIRST], [result()], "vendor"],
    ["an unstamped later frame stays with the send its turn answers", [FIRST], [reply(stamp(FIRST.uuid)), reply()], "send:turn-1"],
    ["an unstamped later frame stays with the vendor turn", [FIRST], [reply(), reply()], "vendor"],
    ["a preamble frame says nothing", [FIRST], [hook()], "unstated"],
    ["a keep-alive's reply is the keep-alive's", [KEEPALIVE], [reply(stamp(KEEPALIVE.uuid))], "send:keepalive-1"],
    ["a reply naming a batch is the batch's last member", [FIRST, SECOND], [reply(stamp(FIRST.uuid, SECOND.uuid))], "send:turn-2"],
  ])("attributes: %s", (_name, sends, messages, expected) => {
    // Arrange
    const ledger = ledgerWith(...sends);
    const leading = messages.slice(0, -1);
    const last = messages.at(-1) as SdkMessage;
    for (const message of leading) ledger.attribute(message);

    // Act
    const verdict = ledger.attribute(last);

    // Assert
    expect(turnOf(verdict)).toBe(expected);
  });

  it("attributes a StartTurn racing a vendor-started turn to each turn by its echo", () => {
    // Arrange: the send is open, and the vendor runs its own turn first.
    const ledger = ledgerWith(FIRST);

    // Act
    const verdicts = [reply(), result(), reply(stamp(FIRST.uuid)), result(stamp(FIRST.uuid))].map((message) =>
      ledger.attribute(message),
    );

    // Assert
    expect(verdicts.map(turnOf)).toEqual(["vendor", "vendor", "send:turn-1", "send:turn-1"]);
  });

  it("matches two quick sends each by its own echo", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    const first = [reply(stamp(FIRST.uuid)), result(stamp(FIRST.uuid))].map((message) => ledger.attribute(message));
    ledger.sent(SECOND);

    // Act
    const second = [reply(stamp(SECOND.uuid)), result(stamp(SECOND.uuid))].map((message) => ledger.attribute(message));

    // Assert
    expect([...first, ...second].map(turnOf)).toEqual([
      "send:turn-1",
      "send:turn-1",
      "send:turn-2",
      "send:turn-2",
    ]);
  });

  it("marks only a first unstamped reply as opening a vendor-started turn", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act
    const opened = [reply(), reply(), result()].map((message) => ledger.attribute(message).openedVendorTurn);

    // Assert
    expect(opened).toEqual([true, false, false]);
  });

  it("ends the running vendor turn on a result", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Act
    ledger.attribute(result());

    // Assert
    expect(ledger.current().kind).toBe("unstated");
  });

  it("closes the send a result answers, so a later echo of it reads as retired", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.attribute(result(stamp(FIRST.uuid)));

    // Act
    const late = ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(turnOf(late)).toBe("send:turn-1:retired");
  });

  it("reports the vendor-started turn ABSORBED when it folds a send in", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.attribute(reply());

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect([turnOf(verdict), verdict.absorbed]).toEqual(["send:turn-1", [{ kind: "vendor" }]]);
  });

  it("reports the other open sends a batch consumed as ABSORBED", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, SECOND);

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid, SECOND.uuid)));

    // Assert
    expect(verdict.absorbed).toEqual([{ kind: "send", send: FIRST }]);
  });

  it("reports the send a turn's echo moved away from as ABSORBED", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, SECOND);
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Act
    const verdict = ledger.attribute(reply(stamp(SECOND.uuid)));

    // Assert
    expect(verdict.absorbed).toEqual([{ kind: "send", send: FIRST }]);
  });

  it("absorbs nothing on an ordinary send's turn", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(verdict.absorbed).toEqual([]);
  });

  it("attributes an echo naming only an unknown uuid to no send", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act
    const verdict = ledger.attribute(reply(stamp(STRANGER)));

    // Assert
    expect(verdict.turn).toEqual({ kind: "unknown", uuids: [STRANGER] });
  });

  it("binds to the last of its own sends in the list when the single field is not its own", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act
    const verdict = ledger.attribute(reply({ user_message_uuid: STRANGER, user_message_uuids: [FIRST.uuid, STRANGER] }));

    // Assert
    expect(turnOf(verdict)).toBe("send:turn-1");
  });

  it("still records the unknown member of a list it bound by another member at ERROR", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    const before = logSinkMark();

    // Act
    ledger.attribute(reply({ user_message_uuid: STRANGER, user_message_uuids: [FIRST.uuid, STRANGER] }));

    // Assert
    expect(logRecordsSince(before).map((record) => [record.level, record.context.unknown_uuids])).toContainEqual([
      "error",
      STRANGER,
    ]);
  });

  it("records an unknown echo at ERROR, naming the uuid", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    const before = logSinkMark();

    // Act
    ledger.attribute(reply(stamp(STRANGER)));

    // Assert
    expect(
      logRecordsSince(before).map((record) => [record.level, record.message, record.context.unknown_uuids]),
    ).toContainEqual([
      "error",
      "a vendor frame echoed a client uuid the shim never sent; it is attributed to no send",
      STRANGER,
    ]);
  });

  it("recognizes a forgotten send's late echo as that send's, retired", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.forget(FIRST.turnId, "the turn was killed");

    // Act
    const verdict = ledger.attribute(result(stamp(FIRST.uuid)));

    // Assert
    expect(turnOf(verdict)).toBe("send:turn-1:retired");
  });

  it("forgets the retired sends past its bound, so their echo is then unknown", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.forget(FIRST.turnId, "retired first");
    for (let index = 0; index < RETIRED_SENDS_REMEMBERED; index++) {
      const send = { uuid: `retired-${index}`, turnId: `turn-r${index}`, keepalive: false };
      ledger.sent(send);
      ledger.forget(send.turnId, "retired");
    }

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(verdict.turn.kind).toBe("unknown");
  });

  it("forgets the running vendor turn when a new query is bound", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.attribute(reply());

    // Act
    ledger.queryBound();

    // Assert
    expect(ledger.current().kind).toBe("unstated");
  });

  it("keeps an open send open across a new query, so a re-delivery is still matched", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);
    ledger.queryBound();

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(turnOf(verdict)).toBe("send:turn-1");
  });

  it("names the open send of a turn", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act / Assert
    expect([ledger.openSendOf("turn-1"), ledger.openSendOf("turn-9")]).toEqual([FIRST, undefined]);
  });

  it("REFUSES a uuid registered twice", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act / Assert
    expect(() => ledger.sent(FIRST)).toThrow(/registered twice/);
  });

  it("attributes a batch answering a member to the batch's first send", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, SECOND);
    ledger.batched(FIRST.turnId, [SECOND.turnId]);

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid, SECOND.uuid)));

    // Assert
    expect(turnOf(verdict)).toBe("send:turn-1");
  });

  it("reports the batch's other members ABSORBED into its first send's turn", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, SECOND);
    ledger.batched(FIRST.turnId, [SECOND.turnId]);

    // Act
    const verdict = ledger.attribute(reply(stamp(FIRST.uuid, SECOND.uuid)));

    // Assert
    expect(verdict.absorbed).toEqual([{ kind: "send", send: SECOND }]);
  });

  it("keeps a batch's later frames on its first send's turn once the member is retired", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, SECOND);
    ledger.batched(FIRST.turnId, [SECOND.turnId]);
    ledger.attribute(reply(stamp(FIRST.uuid, SECOND.uuid)));

    // Act
    const verdict = ledger.attribute(result(stamp(FIRST.uuid, SECOND.uuid)));

    // Assert
    expect([turnOf(verdict), verdict.ended]).toEqual(["send:turn-1", true]);
  });

  it("attributes a batch member answered alone to the member itself", () => {
    // Arrange: the vendor ran the member as its own turn, not beside the first.
    const ledger = ledgerWith(FIRST, SECOND);
    ledger.batched(FIRST.turnId, [SECOND.turnId]);

    // Act
    const verdict = ledger.attribute(reply(stamp(SECOND.uuid)));

    // Assert
    expect(turnOf(verdict)).toBe("send:turn-2");
  });

  it("REFUSES a batch headed by a turn with no open send", () => {
    // Arrange
    const ledger = ledgerWith(SECOND);

    // Act / Assert
    expect(() => ledger.batched(FIRST.turnId, [SECOND.turnId])).toThrow(/heads a prompt batch with no open send/);
  });

  it("REFUSES a batch member with no open send", () => {
    // Arrange
    const ledger = ledgerWith(FIRST);

    // Act / Assert
    expect(() => ledger.batched(FIRST.turnId, [SECOND.turnId])).toThrow(/batched behind turn-1 with no open send/);
  });

  it("REFUSES more open sends than its bound", () => {
    // Arrange
    const ledger = new SendLedger();
    for (let index = 0; index < OPEN_SENDS_BOUND; index++) {
      ledger.sent({ uuid: `open-${index}`, turnId: `turn-o${index}`, keepalive: false });
    }

    // Act / Assert
    expect(() => ledger.sent(SECOND)).toThrow(/cannot open another/);
  });
});

describe("SendLedger first-reply timing", () => {
  /** A ledger on a clock the test sets, recording every first reply. */
  function timedLedger(): { ledger: SendLedger; at: (ms: number) => void; replies: [string, number][] } {
    let now = 0;
    const replies: [string, number][] = [];
    const ledger = new SendLedger(
      (send, seconds) => replies.push([send.turnId, seconds]),
      () => now,
    );
    return { ledger, at: (ms) => (now = ms), replies };
  }

  it("times a prompt from its registration to the first reply that names it, once", () => {
    // Arrange
    const { ledger, at, replies } = timedLedger();
    at(1_000);
    ledger.sent(FIRST);

    // Act
    at(3_500);
    ledger.attribute(streamEvent(stamp(FIRST.uuid)));
    at(4_000);
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(replies).toEqual([["turn-1", 2.5]]);
  });

  it("never times the shim's own keep-alive", () => {
    // Arrange
    const { ledger, replies } = timedLedger();
    ledger.sent(KEEPALIVE);

    // Act
    ledger.attribute(reply(stamp(KEEPALIVE.uuid)));

    // Assert
    expect(replies).toEqual([]);
  });

  it("never times a prompt whose turn ended before any reply", () => {
    // Arrange
    const { ledger, replies } = timedLedger();
    ledger.sent(FIRST);

    // Act: the turn is killed, and a late reply still names its send.
    ledger.forget("turn-1", "killed");
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Assert
    expect(replies).toEqual([]);
  });

  it("never times a result, which is no reply", () => {
    // Arrange
    const { ledger, replies } = timedLedger();
    ledger.sent(FIRST);

    // Act
    ledger.attribute(result(stamp(FIRST.uuid)));

    // Assert
    expect(replies).toEqual([]);
  });

  it("never charges a vendor-started turn's unstamped reply to an open prompt", () => {
    // Arrange
    const { ledger, replies } = timedLedger();
    ledger.sent(FIRST);

    // Act
    ledger.attribute(reply());

    // Assert
    expect(replies).toEqual([]);
  });
});

/**
 * A PEER SEND IS MATCHED BY ITS LIFECYCLE. The vendor echoes nothing on the
 * turn a peer message drives; its `command_lifecycle` names the send instead,
 * and that path is taken for a send marked `peer` alone.
 */
describe("the send ledger's lifecycle path, for peer sends", () => {
  const PEER: Send = { uuid: "00000000-0000-4000-8000-0000000000pe", turnId: "peer-turn", keepalive: false, peer: true };

  const lifecycle = (state: string, uuid: string): SdkMessage =>
    ({ type: "command_lifecycle", command_uuid: uuid, state, uuid: "frame-uuid", session_id: "vendor-1" }) as unknown as SdkMessage;

  /** The verdict's absorbed turns, rendered for a table's expectation. */
  const absorbedOf = (verdict: SendVerdict): string[] =>
    verdict.absorbed.map((turn) => (turn.kind === "send" ? `send:${turn.send.turnId}` : turn.kind));

  it.each<[string, Send[], SdkMessage[], string]>([
    ["a peer send's start between turns makes the running turn its own", [PEER], [lifecycle("started", PEER.uuid)], "send:peer-turn"],
    ["a peer send's unstamped reply belongs to its turn", [PEER], [lifecycle("started", PEER.uuid), reply()], "send:peer-turn"],
    ["a peer send's unstamped result ends its turn", [PEER], [lifecycle("started", PEER.uuid), reply(), result()], "send:peer-turn"],
    ["a peer send's queued frame says nothing", [PEER], [lifecycle("queued", PEER.uuid)], "unstated"],
    ["an ordinary send's start says nothing: it is matched by its echo", [FIRST], [lifecycle("started", FIRST.uuid)], "unstated"],
    ["a start naming a uuid never sent says nothing", [PEER], [lifecycle("started", STRANGER)], "unstated"],
  ])("attributes: %s", (_name, sends, messages, expected) => {
    // Arrange
    const ledger = ledgerWith(...sends);
    for (const message of messages.slice(0, -1)) ledger.attribute(message);

    // Act
    const verdict = ledger.attribute(messages.at(-1) as SdkMessage);

    // Assert
    expect(turnOf(verdict)).toBe(expected);
  });

  it("opens no vendor-started turn for a peer send's unstamped first reply", () => {
    // Arrange
    const ledger = ledgerWith(PEER);
    ledger.attribute(lifecycle("started", PEER.uuid));

    // Act
    const verdict = ledger.attribute(reply());

    // Assert
    expect(verdict.openedVendorTurn).toBe(false);
  });

  it("retires a peer send on its turn's result, so the next unstamped reply is the vendor's own", () => {
    // Arrange
    const ledger = ledgerWith(PEER);
    for (const message of [lifecycle("started", PEER.uuid), reply(), result()]) ledger.attribute(message);

    // Act
    const verdict = ledger.attribute(reply());

    // Assert
    expect([turnOf(verdict), verdict.openedVendorTurn, ledger.openSendOf("peer-turn")]).toEqual(["vendor", true, undefined]);
  });

  it("folds a peer send started inside another send's turn into that turn, concluding the peer send", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, PEER);
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Act
    const verdict = ledger.attribute(lifecycle("started", PEER.uuid));

    // Assert
    expect([turnOf(verdict), absorbedOf(verdict), ledger.openSendOf("peer-turn")]).toEqual([
      "send:turn-1",
      ["send:peer-turn"],
      undefined,
    ]);
  });

  it("takes a vendor-started turn over for a peer send folded into it, absorbing the vendor turn", () => {
    // Arrange
    const ledger = ledgerWith(PEER);
    ledger.attribute(reply());

    // Act
    const verdict = ledger.attribute(lifecycle("started", PEER.uuid));

    // Assert
    expect([turnOf(verdict), absorbedOf(verdict)]).toEqual(["send:peer-turn", ["vendor"]]);
  });

  it("opens a vendor-started turn, at ERROR, for a refused peer send the vendor ran anyway", () => {
    // Arrange
    const ledger = ledgerWith(PEER);
    ledger.forget("peer-turn", "the vendor held it");
    const mark = logSinkMark();

    // Act
    const verdict = ledger.attribute(lifecycle("started", PEER.uuid));

    // Assert
    expect([turnOf(verdict), verdict.openedVendorTurn, logRecordsSince(mark).some((record) => record.level === "error")]).toEqual([
      "vendor",
      true,
      true,
    ]);
  });

  it("leaves the running turn as it is when a refused peer send starts inside it", () => {
    // Arrange
    const ledger = ledgerWith(FIRST, PEER);
    ledger.forget("peer-turn", "the vendor held it");
    ledger.attribute(reply(stamp(FIRST.uuid)));

    // Act
    const verdict = ledger.attribute(lifecycle("started", PEER.uuid));

    // Assert
    expect([turnOf(verdict), verdict.openedVendorTurn]).toEqual(["send:turn-1", false]);
  });

  it("times a peer send's first reply from its registration", () => {
    // Arrange
    let now = 0;
    const replies: [string, number][] = [];
    const ledger = new SendLedger((send, seconds) => replies.push([send.turnId, seconds]), () => now);
    ledger.sent(PEER);
    ledger.attribute(lifecycle("started", PEER.uuid));
    now = 1_500;

    // Act
    ledger.attribute(reply());
    ledger.attribute(reply());

    // Assert
    expect(replies).toEqual([["peer-turn", 1.5]]);
  });
});
