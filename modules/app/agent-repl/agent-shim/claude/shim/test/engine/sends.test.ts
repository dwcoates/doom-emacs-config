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
