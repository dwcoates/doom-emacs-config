/**
 * test/integration/turn.test.ts — StartTurn, WatchAgent, UpdateAgent, KillTurn,
 * ReadHistory.
 *
 * The agent surface is ONE API whether the agent is the main thread or a
 * subagent, and its two invariants run through everything here: ONE TURN IN
 * FLIGHT (structurally, never queued — the daemon is the only queue), and
 * HISTORY IS SERVED FROM THE STORE (never from memory), which is what makes a
 * restarted daemon able to reattach and miss nothing.
 */
import { create, toJsonString } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1, storev1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  agentId,
  allowOnce,
  freshSession,
  openStream, openSessionUpdates,
  pointer,
  promptAgent,
  readHistoryAfter,
  readHistoryFirst,
  readHistoryThrough,
  resumeSession,
  startTurnRequest,
  stopAgent,
  turnId,
  watchAgentRequest,
  workId,
} from "../integration-support/client.js";
import {
  entryFrame,
  entryPrompt,
  historyPage,
  killTurnCause,
  readHistoryKind,
  sessionStarted,
  sessionUpdate,
  startTurnKind,
  stopBashAccepted,
  turnKilled,
  turnStarted,
  updateAccepted,
  updateAgentKind,
  watchAgentEntry,
  watchAgentPage,
} from "../integration-support/expect.js";
import { writtenKeys } from "../integration-support/store.js";
import { KEEPALIVE_PROMPT_MARKER as KEEPALIVE_MARKER } from "../../src/engine/keepalive.js";
import { promptText, readTranscript, userPrompts } from "../integration-support/vendor.js";
import { promptVendorUuid } from "../../src/convert/ids.js";
import { rollBackSessionRequestFor } from "../service/requests.js";

afterEach(cleanupShims);

/** Pull the WatchAgent stream until the turn's terminal frame arrives. */
async function untilTerminal(
  stream: ReturnType<typeof openStream<shimv1.WatchAgentResponse>>,
): Promise<conversationv1.HistoryEntryAt> {
  const frame = await stream.until((f) => {
    if (f.frame.case !== "entry") return false;
    const inner = watchAgentEntry(f).entry?.entry;
    if (inner?.case !== "agentFrame") return false;
    return inner.value.result.case === "success" || inner.value.result.case === "failure";
  });
  return watchAgentEntry(frame);
}

describe("StartTurn", () => {
  test("the answer echoes the turn id, the said and the origin", async () => {
    // ECHO TOKENS: every one of these is the daemon's own value coming back, so
    // a mismatch means the shim adopted something it minted itself.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({
        turn: "turn-echo",
        text: "!md",
        origin: conversationv1.PromptOrigin.WEBAPP_USER_SENT,
      }),
    );

    const prompt = turnStarted(response);
    expect(prompt.id?.value).toBe("turn-echo");
    expect(prompt.origin).toBe(conversationv1.PromptOrigin.WEBAPP_USER_SENT);
    expect(prompt.said?.content?.blocks[0]?.block.case).toBe("text");
    if (prompt.said?.content?.blocks[0]?.block.case === "text") {
      expect(prompt.said.content.blocks[0].block.value.text).toBe("!md");
    }
  });

  test("prompt.agent is the MAIN AgentId — the WatchAgent address", async () => {
    // The main AgentId is the conversation's ORIGINAL vendor session id, and it
    // is what a consumer addresses the turn's stream by.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const prompt = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" })),
    );

    expect(prompt.agent?.value).toBe(started.vendorSessionId);
  });

  test("a fresh session's opening page carries EXACTLY the prompt just delivered", async () => {
    // R15 (RULED): the AgentPrompt row is DURABLE before the page is read, and
    // ONE CALL SUBMITS AND PAINTS -- so the page a fresh session's first
    // StartTurn answers with already contains the prompt that opened the turn,
    // and nothing else. An empty page here would mean the consumer's first
    // paint was missing the turn it had just started. `floor` still says there
    // is no older history, which is a different statement from "no page".
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const page = response.result.value.page;
    expect(page?.entries.length).toBe(1);
    expect(page?.boundary.case).toBe("floor");
    // THE ENTRY IS THAT PROMPT, not merely a prompt: its TurnId and its text
    // are the ones this call carried. "A user_prompt is on the page" would pass
    // on a shim that served some other turn's question.
    const served = entryPrompt(page?.entries[0] ?? create(conversationv1.HistoryEntryAtSchema, {}));
    expect(served?.id?.value).toBe("t1");
    const blocks = served?.said?.content?.blocks ?? [];
    expect(blocks.map((block) => (block.block.case === "text" ? block.block.value.text : ""))).toEqual([
      "!md",
    ]);
    // AND THE FILE PLANE AGREES. Both values above are the shim's own; the
    // vendor's transcript is the independent witness that the prompt it served
    // is the prompt it actually delivered.
    expect(userPrompts(readTranscript(shim.dirs, started.vendorSessionId)).map(promptText)).toEqual([
      "!md",
    ]);
  });

  test("a later turn's page carries the previous turn's settled entries, newest first", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(first);
    first.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const entries = response.result.value.page?.entries ?? [];
    expect(entries.length).toBeGreaterThan(0);
    // NEWEST FIRST, ASSERTED WITHOUT READING THE POINTER. Pointers are OPAQUE:
    // the store mints them and only it may interpret them, so parsing one as an
    // integer here would build the suite against a store that happens to mint
    // numbers. What "newest first" means on the wire is the SERVED SEQUENCE —
    // the page's own order — against a ground truth this test already knows:
    // the order the two turns were started in. t1's prompt must come AFTER t2's
    // in the served list.
    const promptOrder = entries
      .map(entryPrompt)
      .filter((prompt): prompt is conversationv1.AgentPrompt => prompt !== null)
      .map((prompt) => prompt.id?.value ?? "");
    expect(promptOrder).toEqual(["t2", "t1"]);
    // Every pointer is distinct, which is the only other property a consumer
    // may rely on.
    const served = entries.map((entry) => entry.at?.value ?? "");
    expect(new Set(served).size).toBe(served.length);
  });

  test("the page carries terminal frames AS ENTRIES", async () => {
    // The feed's stop notice has no other source: a terminal that were not an
    // entry would vanish on repaint.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const entries = response.result.value.page?.entries ?? [];
    const terminals = entries.filter((entry) => {
      const frame = entryFrame(entry);
      return frame?.result.case === "success" || frame?.result.case === "failure";
    });
    expect(terminals.length).toBeGreaterThan(0);
  });

  test("the page replays NO start frames", async () => {
    // A settled frame carries the start's facts by the upsert rule, so a
    // replayed start would draw a second, permanently-running copy of the unit.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const startArms = (response.result.value.page?.entries ?? []).filter((entry) => {
      const frame = entryFrame(entry);
      if (frame?.result.case !== "update") return false;
      const update = frame.result.value.update;
      if (update.case !== "activity") return false;
      const item = update.value.item;
      return item.case !== undefined && "result" in item.value
        ? (item.value as { result: { case?: string } }).result.case === "start"
        : false;
    });
    expect(startArms).toEqual([]);
  });

  test("the page carries prompts as user_prompt entries", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const prompts = (response.result.value.page?.entries ?? [])
      .map(entryPrompt)
      .filter((p): p is conversationv1.AgentPrompt => p !== null);
    expect(prompts.map((p) => p.id?.value)).toContain("t1");
  });

  test("R15: the AgentPrompt row is written BEFORE any activity row", async () => {
    // The shim's AgentPrompt row is the ONE served prompt, and it is durably
    // acked before the turn's first activity frame — otherwise a feed can paint
    // an answer above the question it answers.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const keys = writtenKeys(shim.store?.writes() ?? []);
    const promptAt = keys.indexOf("prompt:t1");
    const firstActivityAt = keys.findIndex((key) => key.startsWith("activity:"));
    expect(promptAt).toBeGreaterThanOrEqual(0);
    expect(firstActivityAt).toBeGreaterThan(promptAt);
    watch.close();
  });

  test("a second StartTurn while one is open is refused turn_already_open", async () => {
    // ONE TURN IN FLIGHT, STRUCTURALLY: a second is a daemon fault, refused,
    // never queued. The vendor's queue is never ours.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const second = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    expect(startTurnKind(second)).toBe("turnAlreadyOpen");
  });

  test("StartTurn with no session is refused no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md" }),
    );

    expect(startTurnKind(response)).toBe("noSession");
  });
});

/**
 * The turn-stop taxonomy, as ONE table.
 *
 * `sdk.d.ts` declares FOUR `result` error subtypes; every finer stop is a
 * `TerminalReason` riding the result, and THE PAIRING IS THE CONTRACT — a
 * converter keyed on `subtype` alone reaches four of the sixteen
 * conversation.v1 arms. So the mock's published table is walked here row by
 * row: each `!name` names one (subtype, terminal_reason) pair, and the row
 * states the frame arm that pair must reach. One case per row, so a regression
 * names the arm it broke rather than "the taxonomy".
 *
 * WHAT IS DELIBERATELY NOT ASSERTED HERE, and why:
 *
 * - `AgentUpdate.api_error` is a RECORD-PLANE fact. The vendor writes
 *   `system:api_error` to the transcript, never onto the SDK stream, so the
 *   sidecar is its only producer and no `!api-*` row can put one on this live
 *   WatchAgent. The rows below assert the terminal, which is the whole of what
 *   the stream plane owes.
 * - `!refusal-fallback`, `!refusal-no-fallback` and `!context-window` publish
 *   an `AgentResponseFailure` reason. The refusals arrive as a `fallback`
 *   content block (no text, so no response unit is minted) and
 *   `!context-window` emits no assistant message at all, so those three reasons
 *   have NO producer on this plane today. Recorded as a gap rather than
 *   asserted from an invented fixture.
 */
interface StopRow {
  /** The mock lever. */
  readonly prompt: string;
  /** The turn terminal's frame arm. */
  readonly terminal: "success" | "failure";
  /** The oneof arm inside that terminal. */
  readonly arm: string;
  /**
   * The `ApiRequestFailed` kind the arm must carry.
   *
   * Twelve rows reach `api_request_failed`, so the arm alone says almost
   * nothing: what a consumer acts on is the KIND inside it — whether waiting
   * helps (429/529), whether the user must act (401/402/403), or whether the
   * request itself was wrong (400/413).
   */
  readonly apiKind?: string;
  /** True when the row parks until a stop lands (its terminal IS the stop's). */
  readonly needsStop?: boolean;
}

const STOP_TAXONOMY: readonly StopRow[] = [
  // The producer's own turn-ending vocabulary: `error_during_execution` plus a
  // TerminalReason, which is where most of these live.
  { prompt: "!fail-execution", terminal: "failure", arm: "executionError" },
  { prompt: "!fail-max-turns", terminal: "failure", arm: "maxTurns" },
  { prompt: "!fail-budget", terminal: "failure", arm: "budgetExhausted" },
  {
    prompt: "!fail-structured-output",
    terminal: "failure",
    arm: "structuredOutputRetryExhausted",
  },
  { prompt: "!fail-blocking-limit", terminal: "failure", arm: "blockingLimit" },
  { prompt: "!fail-rapid-refill", terminal: "failure", arm: "rapidRefillBreaker" },
  { prompt: "!fail-prompt-too-long", terminal: "failure", arm: "promptTooLong" },
  { prompt: "!fail-image", terminal: "failure", arm: "imageError" },
  { prompt: "!fail-model", terminal: "failure", arm: "modelError" },
  {
    prompt: "!fail-malformed-tool-use",
    terminal: "failure",
    arm: "malformedToolUseExhausted",
  },
  { prompt: "!fail-tool-deferred", terminal: "failure", arm: "toolDeferred" },
  {
    prompt: "!fail-tool-deferred-unavailable",
    terminal: "failure",
    arm: "toolDeferredUnavailable",
  },
  { prompt: "!fail-turn-setup", terminal: "failure", arm: "turnSetupFailed" },
  { prompt: "!fail-stop-hook", terminal: "failure", arm: "stopHookPrevented" },
  { prompt: "!fail-hook-stopped", terminal: "failure", arm: "hookStopped" },
  // UNGROUNDED ARM, ASSERTED AS IT ACTUALLY LANDS: no `TerminalReason` names
  // continuation-prevention, so the two declared prevent-continuation signals
  // ride a `stop_hook_prevented` terminal and
  // `AgentFailure.continuation_prevented` has no producer at all.
  { prompt: "!fail-continuation-prevented", terminal: "failure", arm: "stopHookPrevented" },
  // `aborted_tools` is the ONE reason that is not a failure: the tools were
  // aborted because a user stopped them, which is an interruption.
  { prompt: "!fail-aborted-tools", terminal: "success", arm: "interrupted" },
  // The API family: twelve reasons, ONE arm, distinguished by the taxonomy the
  // arm carries (asserted per-kind in the test that follows).
  { prompt: "!api-400", terminal: "failure", arm: "apiRequestFailed", apiKind: "invalidRequest" },
  { prompt: "!api-401", terminal: "failure", arm: "apiRequestFailed", apiKind: "authenticationFailed" },
  { prompt: "!api-403", terminal: "failure", arm: "apiRequestFailed", apiKind: "permissionDenied" },
  { prompt: "!api-404", terminal: "failure", arm: "apiRequestFailed", apiKind: "notFound" },
  { prompt: "!api-413", terminal: "failure", arm: "apiRequestFailed", apiKind: "requestTooLarge" },
  { prompt: "!api-429", terminal: "failure", arm: "apiRequestFailed", apiKind: "rateLimited" },
  { prompt: "!api-500", terminal: "failure", arm: "apiRequestFailed", apiKind: "internal" },
  { prompt: "!api-529", terminal: "failure", arm: "apiRequestFailed", apiKind: "overloaded" },
  // THREE KINDS NO STATUS CAN NAME. `billing_error`, `oauth_org_not_allowed`
  // and `max_output_tokens` are separated from their neighbours only by the
  // vendor's own error CLASS — 402 is shared, the org refusal is an ordinary
  // 403 and the output ceiling carries no status at all — so the class the
  // vendor states on its error records is what puts each in its own arm.
  { prompt: "!api-billing", terminal: "failure", arm: "apiRequestFailed", apiKind: "billingError" },
  {
    prompt: "!api-oauth-org",
    terminal: "failure",
    arm: "apiRequestFailed",
    apiKind: "oauthOrgNotAllowed",
  },
  {
    prompt: "!api-max-output",
    terminal: "failure",
    arm: "apiRequestFailed",
    apiKind: "maxOutputTokens",
  },
  { prompt: "!api-unmodeled", terminal: "failure", arm: "apiRequestFailed", apiKind: "unmodeled" },
  // The response-level stops. `!max-tokens` keeps the partial text and the turn
  // itself completes; `!context-window` and the two refusals end the turn.
  { prompt: "!max-tokens", terminal: "success", arm: "completed" },
  { prompt: "!refusal-fallback", terminal: "success", arm: "completed" },
  // `model_error`: the refusal with no fallback ends on the model's own
  // failure, which is the reason the result carries.
  { prompt: "!refusal-no-fallback", terminal: "failure", arm: "modelError" },
  { prompt: "!context-window", terminal: "failure", arm: "promptTooLong" },
  // The interrupt lands INSIDE a tool call, so the turn parks until the stop.
  { prompt: "!interrupt", terminal: "success", arm: "interrupted", needsStop: true },
];

describe("the turn-stop taxonomy", () => {
  test.each(STOP_TAXONOMY)("$prompt reaches its declared terminal", async (row) => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: row.prompt }));
    if (row.needsStop === true) {
      // The park is armed before the frame that announces it, so waiting for
      // any entry is enough to know the interrupt has something to land on.
      await watch.until((frame) => frame.frame.case === "entry");
      updateAccepted(await shim.clients.h1.updateAgent(stopAgent()));
    }
    const terminal = await untilTerminal(watch);

    const frame = entryFrame(terminal);
    expect(frame?.result.case).toBe(row.terminal);
    if (frame?.result.case === "failure") {
      expect(frame.result.value.failure.case).toBe(row.arm);
    } else if (frame?.result.case === "success") {
      expect(frame.result.value.outcome.case).toBe(row.arm);
    }
    if (row.apiKind !== undefined) {
      if (frame?.result.case !== "failure" || frame.result.value.failure.case !== "apiRequestFailed") {
        throw new Error("the api row did not end AgentFailure.api_request_failed");
      }
      expect(frame.result.value.failure.value.kind.case).toBe(row.apiKind);
    }
    watch.close();
  });

  test("an api_error is MID-TURN EVIDENCE on the record plane, never a terminal", async () => {
    // The two are distinct facts with distinct producers. The vendor writes
    // `system:api_error` to the TRANSCRIPT and never onto the SDK stream, so
    // the sidecar is the only producer of `AgentUpdate.api_error` and this live
    // stream carries the terminal alone. A converter that raised the evidence
    // into a terminal would end turns that recovered; one that read the
    // terminal as evidence would draw a failed turn as still running.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!api-429" }));
    const terminal = await untilTerminal(watch);

    // The EVIDENCE, on the plane that has it.
    const records = readTranscript(shim.dirs, started.vendorSessionId);
    expect(records.filter((record) => record.subtype === "api_error").length).toBe(1);
    // The TERMINAL, on this one, carrying the same classification.
    const frame = entryFrame(terminal);
    if (frame?.result.case !== "failure" || frame.result.value.failure.case !== "apiRequestFailed") {
      throw new Error("the turn did not end AgentFailure.api_request_failed");
    }
    expect(frame.result.value.failure.value.kind.case).toBe("rateLimited");
    // And no api_error page line rode the live stream — that arm is the
    // sidecar's, and a second producer would double every recorded failure.
    expect(
      watch.frames().some((served) => {
        if (served.frame.case !== "entry") return false;
        const agentFrame = entryFrame(watchAgentEntry(served));
        return (
          agentFrame?.result.case === "update" &&
          agentFrame.result.value.update.case === "apiError"
        );
      }),
    ).toBe(false);
    watch.close();
  });
});

describe("WatchAgent", () => {
  test("a consumer that cancels over HTTP/1.1 leaves no error behind when its book grows and the shim stands down", async () => {
    // THE !cancel-all SHUTDOWN SHAPE. The consumer cancels its standing watch,
    // the book keeps growing, and the shim stands down. The departure used to
    // be learned only when the adapter wrote the next frame onto the destroyed
    // response, which reached the route as an "exception no handler
    // anticipated" at ERROR; a watch parked at a yield was never learned of,
    // and the teardown waited out its conclusion budget at ERROR.
    // Arrange.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    watchAgentPage(await watch.next());
    watch.close();
    await shim.log.record(
      (record) => record.message === "the WatchAgent consumer departed; its tail is closed and nothing more is written to it",
    );

    // Act.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await shim.standDown();

    // Assert.
    const loud = shim.log
      .records()
      .filter((record) => record.level === "warn" || record.level === "error")
      .map((record) => `${String(record.operation)}: ${record.message}`);
    expect(loud).toEqual([]);
  });

  test("opened before any turn, it opens with an EMPTY page and a floor boundary", async () => {
    // The counterpart to R15's StartTurn page: nothing has been written yet, so
    // this is the one open that legitimately paints nothing. An empty page is
    // still a page -- `floor` says there is no older history, which is a
    // different statement from "no page was served".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());

    expect(page.entries).toEqual([]);
    expect(page.boundary.case).toBe("floor");
    watch.close();
  });

  test("a tailed entry carries the place the shim observed it at", async () => {
    // Arrange.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    // Act.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const first = watchAgentEntry(await watch.next());
    watch.close();

    // Assert.
    expect([first.place.case, (first.place.value?.atMs ?? 0n) > 0n]).toEqual(["recordedPlace", true]);
  });

  test("it opens with a page and then tails one POINTERED entry per write", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const first = watchAgentEntry(await watch.next());

    expect(page.boundary.case).toBe("floor");
    expect(first.at?.value).not.toBe("");
    expect(first.entry).toBeDefined();
    watch.close();
  });

  test("a streamed response's update frames are DELTAS, never cumulative", async () => {
    // `AgentResponseUpdate.new_markdown` is a delta and the accumulator is the
    // daemon's; a cumulative update would have every consumer double the text.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const deltas: string[] = [];
    for (const frame of watch.frames()) {
      if (frame.frame.case !== "entry") continue;
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") continue;
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") continue;
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "update") continue;
      deltas.push(item.value.result.value.newMarkdown);
    }
    expect(deltas.length).toBeGreaterThan(0);
    // No delta contains the one before it: cumulative text would nest.
    for (let index = 1; index < deltas.length; index++) {
      const previous = deltas[index - 1] ?? "";
      const current = deltas[index] ?? "";
      if (previous.length > 0) expect(current.startsWith(previous)).toBe(false);
    }
    watch.close();
  });

  test("the response's terminal carries the WHOLE prose", async () => {
    // Terminals carry wholes and self-correct: a consumer that dropped a delta
    // still ends up with the right text.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const settled = watch.frames().flatMap((frame) => {
      if (frame.frame.case !== "entry") return [];
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return [];
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return [];
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "success") return [];
      return [item.value.result.value];
    });
    // THE TERMINAL SELF-CORRECTS: a consumer that dropped every delta still
    // ends up with the right text, which is only true if the whole IS the
    // concatenation. "Not empty" would pass on a terminal carrying one word.
    const deltas = watch.frames().flatMap((frame) => {
      if (frame.frame.case !== "entry") return [];
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return [];
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return [];
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "update") return [];
      return [item.value.result.value.newMarkdown];
    });
    expect(settled.length).toBe(1);
    expect(deltas.length).toBeGreaterThan(1);
    expect(settled[0]?.prose?.markdown).toBe(deltas.join(""));
    expect(settled[0]?.authorship.case).toBe("fromModel");
    watch.close();
  });

  test("usage rides EXACTLY ONE unit per API response", async () => {
    // Usage rides the AgentActivity ENVELOPE on the first-block unit; absence
    // means "not the carrying unit", never "free".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello there" }));
    await untilTerminal(watch);

    const withUsage = watch.frames().filter((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return update.case === "activity" && update.value.usage !== undefined;
    });
    // One API response in this scenario, so exactly one carrying unit.
    const carryingIds = new Set(
      withUsage.map((frame) => {
        const agentFrame =
          frame.frame.case === "entry" ? entryFrame(frame.frame.value) : null;
        if (agentFrame?.result.case !== "update") return "";
        const update = agentFrame.result.value.update;
        return update.case === "activity" ? (update.value.activityId?.value ?? "") : "";
      }),
    );
    expect(carryingIds.size).toBe(1);
    // THE CARRIER IS THE MESSAGE'S FIRST BLOCK. Unit keys are
    // `activity:<message>:<n>` 0-based, so the carrier's id ends in `:0` — and
    // it is the EARLIEST-POINTERED unit of that message, which is the property
    // a consumer relies on when it draws the cost beside the answer's opening.
    const carrier = [...carryingIds][0] ?? "";
    expect(carrier.endsWith(":0")).toBe(true);
    const message = carrier.slice(0, carrier.lastIndexOf(":"));
    // Served order IS pointer order on one tail, so "earliest-pointered" is the
    // first frame of that message the stream served — no pointer is parsed.
    const ofMessage = watch
      .frames()
      .filter((frame) => frame.frame.case === "entry")
      .map((frame) => {
        const agentFrame = entryFrame(watchAgentEntry(frame));
        if (agentFrame?.result.case !== "update") return "";
        const update = agentFrame.result.value.update;
        return update.case === "activity" ? (update.value.activityId?.value ?? "") : "";
      })
      .filter((id) => id.startsWith(`${message}:`));
    expect(ofMessage[0]).toBe(carrier);
    watch.close();
  });

  test("known_through serves ONLY newer entries", async () => {
    // The caller's own high-water mark: the store remembers nothing about what
    // it served, so catch-up is the caller's statement, not the store's memory.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const terminal = await untilTerminal(first);
    const highWater = terminal.at?.value ?? "";
    first.close();

    const second = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ knownThrough: pointer(highWater) }),
        options,
      ),
    );
    const page = watchAgentPage(await second.next());

    // POINTERS ARE OPAQUE: only the store may interpret one, so "newer" is
    // asserted as the two facts a consumer actually has — the mark itself is
    // not replayed, and every entry the FIRST watch already served (the whole
    // turn, through its terminal) is absent from the catch-up page.
    const alreadySeen = new Set(
      first
        .frames()
        .filter((frame) => frame.frame.case === "entry")
        .map((frame) => watchAgentEntry(frame).at?.value ?? ""),
    );
    expect(alreadySeen.has(highWater)).toBe(true);
    expect(
      page.entries.map((entry) => entry.at?.value ?? "").filter((at) => alreadySeen.has(at)),
    ).toEqual([]);
    second.close();
  });

  test("tail_only opens on an EMPTY page at the floor", async () => {
    // Opening a watch replays no history: the daemon loads pages only when a
    // reader asks for them.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(first);
    first.close();

    const tailOnly = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ tailOnly: true }), options),
    );
    const page = watchAgentPage(await tailOnly.next());

    expect(page.entries).toEqual([]);
    expect(page.boundary.case).toBe("floor");
    tailOnly.close();
  });

  test("a tail_only watch carries only what is written after it opened", async () => {
    // The tail begins after the newest entry as of the open, so the next turn
    // reaches it and none of the turn before.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(first);
    first.close();
    const earlier = new Set(
      first
        .frames()
        .filter((frame) => frame.frame.case === "entry")
        .map((frame) => watchAgentEntry(frame).at?.value ?? ""),
    );
    const tailOnly = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ tailOnly: true }), options),
    );
    await tailOnly.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    await untilTerminal(tailOnly);

    const tailed = tailOnly
      .frames()
      .filter((frame) => frame.frame.case === "entry")
      .map((frame) => watchAgentEntry(frame).at?.value ?? "");
    expect(tailed.length).toBeGreaterThan(0);
    expect(tailed.filter((at) => earlier.has(at))).toEqual([]);
    tailOnly.close();
  });

  test("StartTurn with tail_only answers an EMPTY opening page", async () => {
    // The same opening on the one-call submit-and-paint: the prompt is
    // delivered, and the page carries none of the book.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md", tailOnly: true }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    expect(response.result.value.page?.entries).toEqual([]);
  });

  test("a reattach with known_through misses nothing and doubles nothing", async () => {
    // The daemon restart case: the connection dies mid-turn and the replacement
    // reattaches with its own mark.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const before = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await before.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const seen = watchAgentEntry(await before.next());
    const mark = seen.at?.value ?? "";
    // The connection dies mid-turn.
    before.close();
    const during = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ knownThrough: pointer(mark) }), options),
    );
    // THE TERMINAL IS ON THE CATCH-UP PAGE OR THE TAIL, whichever the writer's
    // pace put it on: a backlog lands in merged batches, so the whole turn can
    // be durable before this reattach opens.
    const caughtUp = watchAgentPage(await during.next());
    const endedOnPage = caughtUp.entries.some((entry) => {
      const frame = entryFrame(entry);
      return frame?.result.case === "success" || frame?.result.case === "failure";
    });
    if (!endedOnPage) await untilTerminal(during);
    during.close();

    const after = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ knownThrough: pointer(mark) }), options),
    );
    const page = watchAgentPage(await after.next());

    // Nothing doubled: the entry already seen is not replayed.
    expect(page.entries.map((entry) => entry.at?.value)).not.toContain(mark);
    // Nothing missed: the turn's terminal is in the catch-up page.
    expect(
      page.entries.some((entry) => {
        const frame = entryFrame(entry);
        return frame?.result.case === "success" || frame?.result.case === "failure";
      }),
    ).toBe(true);
    after.close();
  });

  test("a known_through GAP wider than the store's page serves a page and `more` INTO the gap", async () => {
    // The reattach case where the daemon was away long enough for the book to
    // outgrow one page: the catch-up cannot be served whole, so it is a page
    // plus a boundary that says where to continue. A shim that served only what
    // fit and reported `floor` would have the consumer silently missing the
    // middle of the conversation. The page is the STORE's: shrunk here so two
    // turns outgrow it.
    const shim = await spawnShim({ storePageSize: 2 });
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const mark = watchAgentEntry(await watch.next()).at?.value ?? "";
    await untilTerminal(watch);
    // A second turn, so the gap after `mark` is comfortably more than one entry.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!read" }));
    await untilTerminal(watch);
    watch.close();

    const catchUp = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ knownThrough: pointer(mark) }),
        options,
      ),
    );
    const page = watchAgentPage(await catchUp.next());

    expect(page.entries.length).toBe(2);
    // `more`, not `floor`: the boundary is the caller's handle on the rest of
    // the gap, and it names an entry the page actually served.
    expect(page.boundary.case).toBe("more");
    if (page.boundary.case === "more") {
      expect(page.entries.map((entry) => entry.at?.value)).toContain(
        page.boundary.value.lastEntry?.value,
      );
    }
    catchUp.close();
  });

  test("two subscribers share one book, and one closing mid-burst does not stall the other", async () => {
    // A book is not a queue: every subscriber gets its own tail off the same
    // rows. The failure this guards is a shared cursor or a shared write pump —
    // one consumer going away mid-turn would then stop the other's frames, and
    // the surviving surface would freeze with no error to explain it.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const second = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await second.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    // Both are tailing; one dies partway through the burst.
    await first.next();
    await second.next();
    first.close();
    const terminal = await untilTerminal(second);

    // The survivor reached the turn's end, which is the whole claim.
    const frame = entryFrame(terminal);
    expect(frame?.result.case).toBe("success");
    second.close();
  });

  test("an unknown watch target is refused NOT_FOUND, naming the agent", async () => {
    // INTERIM RULING (ledger, rebuild merge): `WatchAgent` has no refusal arm
    // of its own, so an unknown target is a TRANSPORT refusal — and it is
    // pinned rather than left as "some throw", because a store outage, a
    // cancelled call and an id nobody minted all reach a caller as an
    // exception and only the code tells them apart.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const stream = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ target: agentId("no-such-agent") }),
        options,
      ),
    );
    const failure = await stream.nextOrEnd().then(
      () => undefined,
      (err: unknown) => ConnectError.from(err),
    );

    if (failure === undefined) throw new Error("the unknown target was not refused");
    expect(failure.code).toBe(Code.NotFound);
    expect(failure.message).toContain("no-such-agent");
  });
});

describe("ReadHistory", () => {
  test("first serves the NEWEST page", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const terminal = await untilTerminal(watch);
    watch.close();

    const page = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));

    expect(page.entries[0]?.at?.value).toBe(terminal.at?.value);
  });

  test("after(last_entry) walks older until the floor", async () => {
    const shim = await spawnShim({ storePageSize: 2 });
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();
    const firstPage = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));
    if (firstPage.boundary.case !== "more") {
      throw new Error("the fixture produced too few entries to page over");
    }
    let older = historyPage(
      await shim.clients.h1.readHistory(readHistoryAfter(firstPage.boundary.value.lastEntry ?? pointer("0"))),
    );
    const walked = [...older.entries];
    while (older.boundary.case === "more") {
      older = historyPage(
        await shim.clients.h1.readHistory(readHistoryAfter(older.boundary.value.lastEntry ?? pointer("0"))),
      );
      walked.push(...older.entries);
    }

    expect(older.boundary.case).toBe("floor");
    // "OLDER" WITHOUT READING A POINTER: the walk is disjoint from the page it
    // continued, and it stopped at the floor. Parsing the marks as integers
    // would assert against a store that happens to mint numbers.
    const firstMarks = new Set(firstPage.entries.map((entry) => entry.at?.value ?? ""));
    expect(walked.map((entry) => entry.at?.value ?? "").filter((at) => firstMarks.has(at))).toEqual([]);
    expect(walked.length).toBeGreaterThan(0);
  });

  test("every page of a walk is the store's page, and only the last runs short", async () => {
    // No caller picks a budget: first and after both serve the store's page, so
    // every page but the floor's is exactly the store's size.
    const shim = await spawnShim({ storePageSize: 2 });
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();
    const pages = [historyPage(await shim.clients.h1.readHistory(readHistoryFirst()))];
    for (let last = pages[0]; last?.boundary.case === "more"; last = pages[pages.length - 1]) {
      pages.push(
        historyPage(
          await shim.clients.h1.readHistory(readHistoryAfter(last.boundary.value.lastEntry ?? pointer("0"))),
        ),
      );
    }

    const sizes = pages.map((page) => page.entries.length);

    expect(pages.length).toBeGreaterThan(1);
    expect(sizes.slice(0, -1).every((size) => size === 2)).toBe(true);
    expect(pages[pages.length - 1]?.boundary.case).toBe("floor");
  });

  test("every entry it serves carries the place the shim observed it at", async () => {
    // Arrange.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();

    // Act.
    const page = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));

    // Assert.
    expect(new Set(page.entries.map((entry) => entry.place.case))).toEqual(new Set(["recordedPlace"]));
  });

  test("through reads the book as it stood at an instant", async () => {
    // Arrange: the bound is the place of the book's oldest entry.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();
    const whole = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));
    const oldest = whole.entries[whole.entries.length - 1];
    const bound = oldest?.place.value?.atMs ?? 0n;

    // Act.
    const asItStood = historyPage(await shim.clients.h1.readHistory(readHistoryThrough(bound)));

    // Assert.
    const expected = whole.entries.filter((entry) => (entry.place.value?.atMs ?? 0n) <= bound);
    expect(asItStood.entries.map((entry) => entry.at?.value)).toEqual(expected.map((entry) => entry.at?.value));
  });

  test("through a book the store never heard of is refused unknown_agent", async () => {
    // Arrange.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    // Act.
    const response = await shim.clients.h1.readHistory(
      readHistoryThrough(1_000n, { target: agentId("no-such-agent") }),
    );

    // Assert.
    expect(readHistoryKind(response)).toBe("unknownAgent");
  });

  test("an unknown agent is refused unknown_agent", async () => {
    // THE STORE DECIDES, AND IT SAYS SO IN A TYPED ARM. The shim does not know
    // which books exist — it asks — so this refusal only happens if the store
    // states it. The fake used to serve an empty page for a book nobody had
    // written, which made the shim look wrong when it was the fake that never
    // refused; it is now armed with the arm a real store would send.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    shim.store?.failReads("OpenAgentSession", "invalid_request", "no book named that");

    const response = await shim.clients.h1.readHistory(
      readHistoryFirst({ target: agentId("no-such-agent") }),
    );

    expect(readHistoryKind(response)).toBe("unknownAgent");
  });

  test("a stale pointer is refused stale_pointer", async () => {
    // The pointer is the STORE's to recognize: it minted it, and only it can
    // say the mark names no line of this book.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    // A pointer-bearing read WALKS OLDER through ReadAgentPage; only the
    // pointerless first read goes through OpenAgentSession, so arming the
    // opening verb here would arm one the request never reaches.
    shim.store?.failReads("ReadAgentPage", "stale_pointer", "that mark is not in this book");

    const response = await shim.clients.h1.readHistory(
      readHistoryAfter(pointer("pointer-from-a-previous-store")),
    );

    expect(readHistoryKind(response)).toBe("stalePointer");
  });

  test("the refusal follows the ARM, not the store's prose", async () => {
    // The negative that gives the two above their meaning: a detail saying the
    // opposite of the arm must not change the answer. An earlier reader
    // classified by substring, so a storage failure whose driver text happened
    // to mention a pointer was reported as a stale pointer and the engine
    // re-read a book that was actually unreachable.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    shim.store?.failReads("OpenAgentSession", "storage_failure", "that pointer names no such agent");

    const response = await shim.clients.h1.readHistory(readHistoryFirst());

    expect(readHistoryKind(response)).toBe("storeUnavailable");
  });

  test("an unreachable store is refused store_unavailable", async () => {
    // The shim serves history FROM THE STORE, so a store that is down is a
    // refusal with a cause and never an empty page — an empty page would read
    // as "this conversation has no history".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.store?.close();

    const response = await shim.clients.h1.readHistory(readHistoryFirst());

    expect(readHistoryKind(response)).toBe("storeUnavailable");
  });

  test("an unreachable store also raises a store_unreachable fault", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();

    await shim.store?.close();
    await shim.clients.h1.readHistory(readHistoryFirst());
    const faulted = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "storeUnreachable")
      );
    });

    expect(sessionUpdate(faulted).update.case).toBe("diagnostics");
    watch.close();
  });
});

describe("UpdateAgent", () => {
  test("stop on the main agent during a held turn interrupts it", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.updateAgent(stopAgent());
    const terminal = await untilTerminal(watch);

    updateAccepted(response);
    const frame = entryFrame(terminal);
    expect(frame?.result.case).toBe("success");
    if (frame?.result.case === "success") {
      expect(frame.result.value.outcome.case).toBe("interrupted");
      if (frame.result.value.outcome.case === "interrupted") {
        expect(frame.result.value.outcome.value.cause.case).toBe("byUser");
      }
    }
    watch.close();
  });

  test("stop with nothing running is refused nothing_running", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(stopAgent());

    expect(updateAgentKind(response)).toBe("nothingRunning");
  });

  test("a prompt to an existing subagent is refused not_deliverable", async () => {
    // THE PINNED SDK HAS NO ROUTE to prompt an existing subagent, and the
    // nearest landed arm (nothing_running) would have lied about why.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent" }));
    const spawned = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return false;
      const item = update.value.item;
      return item.case === "subagent" && item.value.result.case === "start";
    });
    const agentFrame = entryFrame(watchAgentEntry(spawned));
    let created = "";
    if (agentFrame?.result.case === "update") {
      const update = agentFrame.result.value.update;
      if (update.case === "activity" && update.value.item.case === "subagent") {
        const subagent = update.value.item.value;
        if (subagent.result.case === "start") {
          created = subagent.result.value.createdAgentId?.value ?? "";
        }
      }
    }

    const response = await shim.clients.h1.updateAgent(
      promptAgent("keep going", agentId(created)),
    );

    expect(updateAgentKind(response)).toBe("notDeliverable");
    watch.close();
  });

  test("an answer with no open ask is refused no_open_ask", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: {
            case: "answer",
            value: create(conversationv1.AgentAnswerSchema, {
              answer: {
                case: "permissionDecision",
                value: create(conversationv1.AgentPermissionDecisionSchema, {
                  ask: create(conversationv1.AgentPermissionIdSchema, { value: "nobody" }),
                  decision: {
                    case: "allowed",
                    value: create(conversationv1.AgentPermissionAllowedSchema, {
                      scope: {
                        case: "once",
                        value: create(conversationv1.AgentPermissionAllowedOnceSchema, {}),
                      },
                    }),
                  },
                }),
              },
            }),
          },
        }),
      }),
    );

    expect(updateAgentKind(response)).toBe("noOpenAsk");
  });

  test("an unknown target agent is refused unknown_agent", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(stopAgent(agentId("no-such-agent")));

    expect(updateAgentKind(response)).toBe("unknownAgent");
  });
});

describe("KillTurn", () => {
  test("with nothing spawned it ends agent_only and the stream concludes interrupted", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );
    const terminal = await untilTerminal(watch);

    expect(turnKilled(response).how.case).toBe("agentOnly");
    const frame = entryFrame(terminal);
    if (frame?.result.case === "success" && frame.result.value.outcome.case === "interrupted") {
      expect(frame.result.value.outcome.value.cause.case).toBe("byUser");
    } else {
      throw new Error("the turn did not conclude AgentSuccess.interrupted.by_user");
    }
    watch.close();
  });

  test("the interrupted terminal records the caller's commanded_by verbatim", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, {
        turn: turnId("t1"),
        force: false,
        commandedBy: create(conversationv1.AgentInterruptedByUserSchema, {
          command: { case: "interjection", value: create(conversationv1.AgentInterruptedByUserInterjectionSchema, {}) },
        }),
      }),
    );
    const terminal = await untilTerminal(watch);

    const frame = entryFrame(terminal);
    if (frame?.result.case !== "success" || frame.result.value.outcome.case !== "interrupted") {
      throw new Error("the turn did not conclude AgentSuccess.interrupted");
    }
    const cause = frame.result.value.outcome.value.cause;
    expect(cause.case === "byUser" ? cause.value.command.case : cause.case).toBe("interjection");
    watch.close();
  });

  test("the shell the stop landed inside SETTLES, interrupted by the user", async () => {
    // WITHOUT THIS THE UNIT NEVER SETTLES. The vendor returns no `tool_result`
    // for a call a stop landed inside — the captured `interrupt` session
    // records the stop as a bare `[Request interrupted by user]` line and
    // nothing else — so the card went on drawing a running shell inside a turn
    // that had ended. Found by the F42 playbook photographing `!bash-hold`.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-hold" }));
    // The call must be GENUINELY OPEN before the stop, or there is nothing for
    // it to cut and the test would pass on an empty claim.
    await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case === "start"
      );
    });

    await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    const cut = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case === "success"
      );
    });
    const frame = entryFrame(watchAgentEntry(cut));
    if (frame?.result.case !== "update") throw new Error("the cut frame is not an update");
    const update = frame.result.value.update;
    if (update.case !== "activity" || update.value.item.case !== "bash") {
      throw new Error("the cut frame is not the shell's");
    }
    const result = update.value.item.value.result;
    if (result.case !== "success" || result.value.outcome.case !== "interrupted") {
      throw new Error("the held shell did not settle on the interrupted arm");
    }
    // A STOP IS NOT A FAILURE OF THE CALL, so it rides the success arm, and the
    // cause is the user rather than a timeout.
    expect(result.value.outcome.value.cause.case).toBe("byUser");
    expect(result.value.command?.line).toBe("tail -f /var/log/system.log");
    watch.close();
  });

  // AN INTERRUPT ENDS ONLY THE SYNCHRONOUS TURN. A non-forced kill used to
  // refuse `live` here, so an interjection could never interrupt a turn that
  // had spawned background work; now it interrupts the turn and the agent the
  // turn spawned runs on.
  test("unforced, it interrupts a turn beside its own live background agent and the agent runs on", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-detached-hold" }));
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") throw new Error("expected the agent's announcement");
    const subagent = detached.result.value.work?.value ?? "";

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );
    const terminal = await untilTerminal(watch);

    expect(turnKilled(response).how.case).toBe("agentOnly");
    const frame = entryFrame(terminal);
    if (frame?.result.case !== "success" || frame.result.value.outcome.case !== "interrupted") {
      throw new Error("the turn did not conclude AgentSuccess.interrupted");
    }
    // AND THE AGENT IS STILL LIVE: its own stop is accepted, which only a live
    // agent's is — one the shim retired answers `unknown_agent`.
    updateAccepted(await shim.clients.h1.updateAgent(stopAgent(agentId(subagent))));
    watch.close();
  });

  test("force ends it, naming the stopped work", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") {
      throw new Error("expected the run's announcement");
    }
    const run = detached.result.value.work?.value ?? "";

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: true }),
    );

    const killed = turnKilled(response);
    expect(killed.how.case).toBe("forced");
    if (killed.how.case !== "forced") throw new Error("the forced kill did not report forced");
    // NAMED, not counted: the consumer learns exactly which work died.
    expect(killed.how.value.stoppedWork.map((work) => work.value)).toEqual([run]);
    watch.close();
  });

  test("a TurnId that is not the open turn is refused not_the_open_turn", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("some-other-turn"), force: false }),
    );

    expect(killTurnCause(response)).toBe("notTheOpenTurn");
  });

  test("with no turn open it is refused no_turn_open", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    expect(killTurnCause(response)).toBe("noTurnOpen");
  });
});

describe("keep-alives", () => {
  // KEEP-ALIVES ARE ENTIRELY SHIM-INTERNAL: nothing keep-alive-shaped exists on
  // the wire (no PromptOrigin value, no rpc, no control-plane signal), so the
  // only producer is the shim's OWN cadence. Its interval is a module constant
  // (fifty-two minutes against subscription billing's ~1-hour cache window),
  // and waiting fifty-two real minutes is the sleep this suite refuses to write
  // — so `main.ts`
  // honors `AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS` under `--fake` and ONLY
  // under `--fake`, which is what makes both obligations below observable.
  // Every wait here is on the shim's own records, never on a clock.

  /** A shim whose keep-alive cadence beats fast enough to observe. */
  const spawnBeating = async (): Promise<Awaited<ReturnType<typeof spawnShim>>> =>
    spawnShim({ env: { AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS: "200" } });

  /** Resolves on the shim's record that it submitted a keep-alive. */
  const keepaliveSubmitted = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
  ): Promise<void> => {
    await shim.log.record((record) => record.context.outcome === "keepalive_submitted");
  };

  /** Resolves once the shim has CLOSED a keep-alive turn, debt booked. */
  const keepaliveTurnClosed = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
  ): Promise<void> => {
    await shim.log.record(
      (record) => record.message === "closed a turn" && record.context.keepalive === true,
    );
  };

  /**
   * Every entry the store was handed that carries `needle` anywhere, on any arm.
   *
   * The mocked vendor ECHOES a prompt into its reply, so every row a keep-alive
   * could produce — its prompt, its answer — carries the keep-alive marker.
   */
  const storedCarrying = (
    shim: Awaited<ReturnType<typeof spawnShim>>,
    needle: string,
  ): storev1.StoreEntry[] =>
    (shim.store?.writes() ?? [])
      .flatMap((request) => request.batch?.entries ?? [])
      .filter((entry) => toJsonString(storev1.StoreEntrySchema, entry).includes(needle));

  /**
   * Resolves once a served row carrying `needle` has reached the store.
   *
   * THE SHIM WRITES IN ORDER, one drain: a row of a real turn submitted AFTER
   * a keep-alive closed landing means every row the shim wrote before it — the
   * keep-alive's included, had it written any — already reached the store. That
   * is what makes an absence assertable.
   */
  const servedLanded = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
    needle: string,
  ): Promise<storev1.StoreEntry> =>
    shim.store?.entryLanded((entry) => {
      if (entry.entry.case !== "agentUpdate") return false;
      const info = entry.entry.value.agentInfo;
      if (info.case !== "serveableFrame") return false;
      const item = info.value.agentItem;
      if (item?.item.case !== "agentFrame") return false;
      return toJsonString(conversationv1.AgentFrameSchema, item.item.value).includes(needle);
    }) ?? Promise.reject(new Error("this shim has no store"));

  test("a keep-alive turn works end to end and stores nothing", async () => {
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await keepaliveTurnClosed(shim);

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "after the keep-alive" })));
    await servedLanded(shim, "after the keep-alive");

    expect(storedCarrying(shim, KEEPALIVE_MARKER)).toEqual([]);
  });

  test("a rewind past a keep-alive lands with nothing of the keep-alive stored", async () => {
    // THE REWIND NEEDS NO ROW: its anchor is an assistant record of the real
    // turn the engine saw go by, and the rewound query reads the vendor's own
    // transcript. So the rewind lands exactly as before while the store holds
    // nothing of what it discarded.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello" }));
    await keepaliveTurnClosed(shim);
    const landed = shim.log.record(
      (record) =>
        record.message === "the keep-alive rewind LANDED: the vendor resumed at the anchor and is answering the real prompt",
    );

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "and again" })));
    await landed;
    await servedLanded(shim, "and again");

    expect(storedCarrying(shim, KEEPALIVE_MARKER)).toEqual([]);
  });

  test("no keep-alive prompt appears in any page", async () => {
    // Nothing of a keep-alive is stored, so no page can return one.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await keepaliveSubmitted(shim);

    const page = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));

    const said = page.entries
      .map(entryPrompt)
      .flatMap((prompt) => prompt?.said?.content?.blocks ?? [])
      .map((block) => (block.block.case === "text" ? block.block.value.text : ""));
    expect(said.some((text) => text.startsWith(KEEPALIVE_MARKER))).toBe(false);
  });

  test("a real prompt after keep-alives is delivered with the context rolled back", async () => {
    // A real prompt must never build on keep-alive context, so the query is
    // replaced by one that resumes only THROUGH the last real record.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello" }));
    // THE DEBT IS BOOKED AT THE KEEP-ALIVE TURN'S CLOSE, not at its submission.
    // A StartTurn arriving while that turn is still open waits for it inside
    // the shim, so this wait is only what makes the rewind's record the one
    // this test asserts on.
    await keepaliveTurnClosed(shim);

    const rewound = shim.log.record((record) => record.context.resume_session_at !== undefined);
    // What the VENDOR received, not merely what the shim intended: the mock
    // records the rewind target it was handed, and the shim's own record is no
    // evidence the value ever reached the query.
    const atVendor = shim.log.record(
      (record) => record.context.vendor_resume_session_at !== undefined,
    );
    // The turn is ACCEPTED, which is the half a refusal would silently take
    // away: a refused StartTurn never reaches the rewind at all.
    turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "and again" })),
    );
    const record = await rewound;

    expect(record.context.discarded_keepalive_turns).toBeDefined();
    expect((await atVendor).context.vendor_resume_session_at).toBe(
      record.context.resume_session_at,
    );
  });

  test("the rewind target is an ASSISTANT record of the transcript", async () => {
    // THE REGRESSION OF 2026-09-14. The anchor used to be whatever message last
    // carried a uuid, and `system:init` and `result` both carry one — so after
    // a resume whose only non-keep-alive messages were the opening's, the shim
    // handed the vendor a uuid that names no record. The vendor exited 1 with
    // `No message found with message.uuid of: 19e047a0-…` and the prompt was
    // lost. The target must be a record the transcript actually holds, and an
    // assistant one, which is the only kind `resumeSessionAt` declares.
    const shim = await spawnBeating();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello" }));
    await keepaliveTurnClosed(shim);
    const atVendor = shim.log.record(
      (record) => record.context.vendor_resume_session_at !== undefined,
    );

    turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "and again" })),
    );
    const target = (await atVendor).context.vendor_resume_session_at;

    const named = readTranscript(shim.dirs, started.vendorSessionId).find(
      (record) => record.uuid === target,
    );
    expect(named?.type).toBe("assistant");
  });

  test("a keep-alive stores nothing when the vendor runs a turn of its own first", async () => {
    // THE 2026-09-23 LEAK: a turn the vendor ran by itself ended first, its
    // result closed the keep-alive, and the keep-alive's answer was served.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" }));
    await keepaliveTurnClosed(shim);

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "after the keep-alive" })));
    await servedLanded(shim, "after the keep-alive");

    expect(storedCarrying(shim, KEEPALIVE_MARKER)).toEqual([]);
  });

  test("a keep-alive the first prompt carried costs the next keep-alive no rewind", async () => {
    // THE E2E FLAKE OF 2026-10-03. The cadence starts with the session, so a
    // StartTurn landing after the first beat finds a keep-alive with NO anchor
    // to rewind to, and the prompt carries it. That keep-alive lies behind the
    // real turn's anchor; counting it as debt made the next keep-alive replace
    // the query, and the vendor's turn queued on the replaced query never ran.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await keepaliveTurnClosed(shim);
    const vendorTurnRan = shim.log.record(
      (record) => record.message === "fake vendor runs a turn of its OWN before the next send; nothing in it is stamped",
    );

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" })));
    await vendorTurnRan;

    const rewoundBeforeAKeepalive = shim.log
      .records()
      .filter((record) => record.context.before === "keepalive")
      .map((record) => record.context);
    expect(rewoundBeforeAKeepalive).toEqual([]);
  });

  test("the vendor's own turn ahead of a keep-alive is still served", async () => {
    // The fix must not overcorrect: a turn nobody's send started is real
    // conversation, and hiding it with the keep-alive would lose it.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" }));

    const answer = await servedLanded(shim, "A background task finished.");

    expect(answer.upsertKey).not.toBe("");
  });

  test("the vendor's own turn is adopted: a VENDOR_STARTED prompt row opens it and its answer carries its id", async () => {
    // A TURN NOBODY'S SEND STARTED IS A REAL TURN. The shim mints its id and
    // writes a VENDOR_STARTED prompt row as its first row, so the daemon learns
    // it opened and its rows and terminal are attributable to it.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" }));

    const answer = await servedLanded(shim, "A background task finished.");

    const opened = storedCarrying(shim, "PROMPT_ORIGIN_VENDOR_STARTED");
    expect(opened).toHaveLength(1);
    expect(opened[0]?.turn?.value).toMatch(/^adopted-/);
    expect(answer.turn?.value).toBe(opened[0]?.turn?.value);
  });

  /** The message the span invariant's refusal is recorded under. */
  const REFUSED = "the keep-alive rewind is REFUSED";

  /** Resolves on the COUNT-th distinct record matching `predicate`. */
  const nthRecord = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
    count: number,
    predicate: (record: { message: string; context: Record<string, unknown> }) => boolean,
  ) => {
    const seen = new Set<unknown>();
    return shim.log.record((record) => {
      if (predicate(record)) seen.add(record);
      return seen.size >= count;
    });
  };

  /** True on the record of a rewind performed before a keep-alive beat. */
  const rewoundBeforeBeat = (record: { message: string; context: Record<string, unknown> }): boolean =>
    record.message.startsWith("REWINDING the vendor context") && record.context.before === "keepalive";

  test("a task completing beside a keep-alive anchors the next rewind, which keeps its served turn", async () => {
    // A completed task's turn is real conversation: it anchors (ruled
    // 2026-10-06), so the rewind after it discards only the keep-alive.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" }));

    const rewound = await shim.log.record(rewoundBeforeBeat);

    expect([
      String(rewound.context.anchor_turn_id).startsWith("adopted-"),
      (rewound.context.discarded_span as { kind: string }[]).map((turn) => turn.kind),
    ]).toEqual([true, ["keepalive"]]);
  });

  test("a task completing beside a keep-alive trips no refusal, and the next prompt is served", async () => {
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" }));
    await shim.log.record(rewoundBeforeBeat);

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "after the completed task" })));
    await servedLanded(shim, "after the completed task");

    expect(shim.log.records().filter((record) => record.message.startsWith(REFUSED))).toEqual([]);
  });

  test("a stop every rewind replays never loops: each rewind resumes at the same real anchor", async () => {
    // THE SHIP-GNS LOOP (2026-10-02), driven by the mock's own replay lever.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!stop-on-rewind" }));

    await nthRecord(shim, 3, rewoundBeforeBeat);

    const anchors = shim.log.records().filter(rewoundBeforeBeat).map((record) => record.context.resume_session_at);
    expect(new Set(anchors).size).toBe(1);
  });

  test("a stop every rewind replays stores no turn", async () => {
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!stop-on-rewind" }));
    await nthRecord(
      shim,
      2,
      (record) => record.message === "fake vendor replays a task STOPPED by the rewind and answers it in a turn of its OWN before the next send",
    );

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "after the replays" })));
    await servedLanded(shim, "after the replays");

    expect([
      storedCarrying(shim, "PROMPT_ORIGIN_VENDOR_STARTED"),
      storedCarrying(shim, "The background task was stopped."),
    ]).toEqual([[], []]);
  });

  test("a stop every rewind replays never trips the span invariant", async () => {
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!stop-on-rewind" }));
    await nthRecord(shim, 2, rewoundBeforeBeat);

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "after the replays" })));
    await servedLanded(shim, "after the replays");

    expect(shim.log.records().filter((record) => record.message.startsWith(REFUSED))).toEqual([]);
  });

  test("a restarted shim holds no anchor: its keep-alives are carried, never rewound, until a real reply", async () => {
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const watch = openStream((options) => first.clients.h1.watchAgent(watchAgentRequest(), options));
    await watch.next();
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "before the restart" }));
    await untilTerminal(watch);
    watch.close();
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs, env: { AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS: "200" } });
    sessionStarted(await second.clients.h1.startSession(resumeSession(started.vendorSessionId)));
    await nthRecord(second, 2, (record) => record.message === "closed a turn" && record.context.keepalive === true);

    expect(second.log.records().filter((record) => record.message.startsWith("REWINDING the vendor context"))).toEqual([]);
  });

  test("a restarted shim's first real reply anchors the next rewind", async () => {
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const watch = openStream((options) => first.clients.h1.watchAgent(watchAgentRequest(), options));
    await watch.next();
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "before the restart" }));
    await untilTerminal(watch);
    watch.close();
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;
    const before = new Set(readTranscript(first.dirs, started.vendorSessionId).map((record) => record.uuid));
    const second = await spawnShim({ reuse: first.dirs, env: { AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS: "200" } });
    sessionStarted(await second.clients.h1.startSession(resumeSession(started.vendorSessionId)));
    const resumedWatch = openStream((options) => second.clients.h1.watchAgent(watchAgentRequest(), options));
    await resumedWatch.next();
    turnStarted(await second.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "after the restart" })));
    await untilTerminal(resumedWatch);
    resumedWatch.close();

    const rewound = await second.log.record(rewoundBeforeBeat);

    // An assistant record the RESUMED shim saw: none from before the restart.
    const named = readTranscript(second.dirs, started.vendorSessionId).find(
      (record) => record.uuid === rewound.context.resume_session_at,
    );
    expect([named?.type, before.has(rewound.context.resume_session_at as string)]).toEqual(["assistant", false]);
  });

  test("a StartTurn sent the moment a keep-alive is submitted opens its turn exactly once", async () => {
    // THE KEEP-ALIVE IS INVISIBLE OUTSIDE THE SHIM. A StartTurn landing while
    // the keep-alive is still open is not refused: it waits inside the shim
    // and is accepted, and the vendor receives its prompt exactly once.
    const shim = await spawnBeating();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await keepaliveSubmitted(shim);

    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "during the keep-alive" })));
    await servedLanded(shim, "during the keep-alive");

    const delivered = userPrompts(readTranscript(shim.dirs, started.vendorSessionId)).filter((record) =>
      promptText(record).includes("during the keep-alive"),
    );
    expect(delivered).toHaveLength(1);
  });

  test("a real prompt's transcript record carries NO keep-alive marker", async () => {
    // The half of the yield obligation that IS observable: the shim's own
    // prompts are marked, and a daemon-submitted prompt must never be.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!keepalive" }));
    await untilTerminal(watch);

    const prompts = userPrompts(readTranscript(shim.dirs, started.vendorSessionId));
    expect(prompts.length).toBeGreaterThan(0);
    expect(prompts.every((record) => !promptText(record).startsWith(KEEPALIVE_MARKER))).toBe(true);
    watch.close();
  });
});

/**
 * A REPLY IS MATCHED TO THE SEND THAT CAUSED IT BY ID (owner ruling 2026-09-28).
 *
 * The mocked vendor delivers the frames in the racing order: `!queue-vendor-turn`
 * queues a turn the vendor runs ON ITS OWN, unstamped, ahead of the next send's
 * own turn. So the next StartTurn's send lands in the vendor's queue, the
 * vendor answers its own turn first, and only then the send's. Matching by
 * arrival charged the vendor's turn to the StartTurn and adopted the
 * StartTurn's real answer as a turn nobody started.
 */
describe("replies matched to their send by id", () => {
  /** A served agent frame carrying `needle` that reached the store. */
  const servedCarrying = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
    needle: string,
  ): Promise<storev1.StoreEntry> =>
    shim.store?.entryLanded((entry) => {
      if (entry.entry.case !== "agentUpdate") return false;
      const info = entry.entry.value.agentInfo;
      if (info.case !== "serveableFrame") return false;
      const item = info.value.agentItem;
      if (item?.item.case !== "agentFrame") return false;
      return toJsonString(conversationv1.AgentFrameSchema, item.item.value).includes(needle);
    }) ?? Promise.reject(new Error("this shim has no store"));

  /** The agent frame a stored entry carries, if it carries one. */
  const storedFrame = (entry: storev1.StoreEntry): conversationv1.AgentFrame | undefined => {
    if (entry.entry.case !== "agentUpdate") return undefined;
    const info = entry.entry.value.agentInfo;
    if (info.case !== "serveableFrame") return undefined;
    const item = info.value.agentItem;
    return item?.item.case === "agentFrame" ? item.item.value : undefined;
  };

  /** Every VENDOR_STARTED prompt row the store was handed. */
  const vendorStartedRows = (shim: Awaited<ReturnType<typeof spawnShim>>): storev1.StoreEntry[] =>
    (shim.store?.writes() ?? [])
      .flatMap((request) => request.batch?.entries ?? [])
      .filter((entry) => toJsonString(storev1.StoreEntrySchema, entry).includes("PROMPT_ORIGIN_VENDOR_STARTED"));

  /** Start t1, which queues a vendor turn, and wait for t1 to close. */
  const queueAVendorTurn = async (shim: Awaited<ReturnType<typeof spawnShim>>): Promise<void> => {
    const closed = shim.log.record((record) => record.message === "closed a turn" && record.context.turn_id === "t1");
    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!queue-vendor-turn" })));
    await closed;
  };

  test("the vendor's own turn racing a StartTurn is served under its adopted id", async () => {
    // Arrange
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await queueAVendorTurn(shim);

    // Act
    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "the racing prompt" })));
    const vendorAnswer = await servedCarrying(shim, "A background task finished.");
    await servedCarrying(shim, "echo: the racing prompt");

    // Assert
    const adopted = vendorStartedRows(shim);
    expect([adopted.length, vendorAnswer.turn?.value === adopted[0]?.turn?.value]).toEqual([1, true]);
  });

  test("the StartTurn racing the vendor's own turn is answered under its own turn id", async () => {
    // Arrange
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await queueAVendorTurn(shim);

    // Act
    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "the racing prompt" })));
    const answer = await servedCarrying(shim, "echo: the racing prompt");

    // Assert
    expect(answer.turn?.value).toBe("t2");
  });

  test("the StartTurn racing the vendor's own turn ends on its own result, naming its own answer", async () => {
    // Arrange
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await queueAVendorTurn(shim);
    const terminalOfT2 = shim.store?.entryLanded((entry) => {
      const frame = storedFrame(entry);
      return entry.turn?.value === "t2" && frame?.result.case === "success";
    });

    // Act
    turnStarted(await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "the racing prompt" })));
    const answer = storedFrame(await servedCarrying(shim, "echo: the racing prompt"));
    const terminal = storedFrame((await terminalOfT2) ?? create(storev1.StoreEntrySchema, {}));

    // Assert
    const answerUnit =
      answer?.result.case === "update" && answer.result.value.update.case === "activity"
        ? answer.result.value.update.value.activityId?.value
        : undefined;
    const named =
      terminal?.result.case === "success" && terminal.result.value.outcome.case === "completed"
        ? terminal.result.value.outcome.value.answer?.value
        : undefined;
    expect([answerUnit !== undefined, named]).toEqual([true, answerUnit]);
  });
});

describe("scope, arms and ordering the verbs owe", () => {
  test("KillTurn kills THIS TURN ONLY: an earlier turn's live run survives", async () => {
    // THE KILL IS SCOPED TO A TURN, and the refusal set is transitive only over
    // the turn's OWN spawn set. t1 left a shell running; t2 is killed while it
    // holds; a kill that reached across turns would destroy work the user never
    // asked to stop and that nothing in the response named.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") throw new Error("expected the run's announcement");
    const run = detached.result.value.work?.value ?? "";
    await untilTerminal(watch);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t2"), force: false }),
    );

    // t2 had nothing of its own, so it ends agent_only rather than refusing.
    expect(turnKilled(response).how.case).toBe("agentOnly");
    // AND t1'S RUN IS STILL LIVE: StopBash answers it, which only a live run
    // does — an already-concluded one answers `already_ended`.
    stopBashAccepted(
      await shim.clients.h1.stopBash(create(shimv1.StopBashRequestSchema, { work: workId(run) })),
    );
    watch.close();
  });

  // THE INCIDENT THIS PINS: an interjection's unforced kill of a later turn
  // took down every background agent earlier turns had spawned, because the
  // vendor's interrupt fails closed unless the per-task stop is declared.
  test("an unforced KillTurn spares a background agent an EARLIER turn spawned", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!subagent-detached-utterance" }),
    );
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") throw new Error("expected the agent's announcement");
    const subagent = detached.result.value.work?.value ?? "";
    await untilTerminal(watch);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t2"), force: false }),
    );
    await untilTerminal(watch);

    expect(turnKilled(response).how.case).toBe("agentOnly");
    // AND t1'S AGENT IS STILL LIVE: its own stop is accepted, which only a live
    // agent's is — one the shim retired answers `unknown_agent`.
    updateAccepted(await shim.clients.h1.updateAgent(stopAgent(agentId(subagent))));
    watch.close();
  });

  test("UpdateAgent.stop targeted at a LIVE detached subagent concludes it stopped_by_user", async () => {
    // ONE API WHETHER THE AGENT IS THE MAIN THREAD OR A SUBAGENT: the same verb
    // addresses the detached agent by its own AgentId, and the vendor's
    // `task_notification{stopped}` is what settles it. The terminal is a page
    // line of the SPAWNING agent's book, by the flatness rule — a subagent's
    // work is never nested in its spawn, and its unit lives where the spawn is.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!subagent-detached-live" }),
    );
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") throw new Error("expected the agent's announcement");
    const subagent = detached.result.value.work?.value ?? "";

    const response = await shim.clients.h1.updateAgent(stopAgent(agentId(subagent)));
    const settled = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.activityId?.value === subagent &&
        update.value.item.case === "subagent" &&
        update.value.item.value.result.case === "failure"
      );
    });

    updateAccepted(response);
    const agentFrame = entryFrame(watchAgentEntry(settled));
    if (agentFrame?.result.case !== "update") throw new Error("expected the settled frame");
    const update = agentFrame.result.value.update;
    if (update.case !== "activity" || update.value.item.case !== "subagent") {
      throw new Error("expected a subagent activity");
    }
    const subagentUnit = update.value.item.value;
    if (subagentUnit.result.case !== "failure") throw new Error("expected the failure arm");
    expect(subagentUnit.result.value.cause.case).toBe("stoppedByUser");
    watch.close();
  });

  test("StartTurn after the query DIED is refused query_dead", async () => {
    // `!query-eof` ends the vendor's iterable with no result, so the session
    // survives and the QUERY does not. The next prompt has nothing to go to,
    // and saying so is different from `no_session`: the session is right there.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!query-eof" }));
    // The shim's own record that the query died: waiting on it, never a timer.
    await watch.until((frame) => sessionUpdate(frame).update.case === "queryDied");

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    expect(startTurnKind(response)).toBe("queryDead");
    watch.close();
  });

  test.todo(
    "StartTurn is refused vendor_refused when the prompt queue rejects the submission — " +
      "UNREACHABLE without a mock lever: the queue only throws once it is CLOSED, and every path " +
      "that closes it also clears the query, which makes the refusal query_dead instead. Needs a " +
      "fake-vendor lever whose streamInput refuses one submission with the query still alive.",
  );

  test("KillTurn before StartSession is refused no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    expect(killTurnCause(response)).toBe("noSession");
  });

  test("UpdateAgent before StartSession is refused no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.updateAgent(stopAgent());

    expect(updateAgentKind(response)).toBe("noSession");
  });

  test("an answer arriving AFTER the turn was killed is refused no_open_ask", async () => {
    // The teardown resolved the callback as denied, so the ask is gone by the
    // time the consumer's answer lands. It is refused with the arm that says
    // there is nothing to answer — and it must not crash the shim, which is the
    // half a refusal arm alone does not state.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await stream.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-hold" }));
    const ask = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return update.case === "permission" && update.value.result.case === "start";
    });
    const agentFrame = entryFrame(watchAgentEntry(ask));
    if (agentFrame?.result.case !== "update" || agentFrame.result.value.update.case !== "permission") {
      throw new Error("expected the permission ask");
    }
    const askId = agentFrame.result.value.update.value.id?.value ?? "";
    await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );
    await untilTerminal(stream);

    const response = await shim.clients.h1.updateAgent(
      allowOnce(create(conversationv1.AgentPermissionIdSchema, { value: askId })),
    );

    expect(updateAgentKind(response)).toBe("noOpenAsk");
    // AND THE SHIM IS STILL THERE: a further round trip resolves, which a
    // process that had crashed on the late answer could not do.
    const after = await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    expect(after.result.case).toBe("success");
    stream.close();
  });

  test("R15: the first activity frame reaches WatchAgent only AFTER the prompt row is acked", async () => {
    // ONE CALL SUBMITS AND PAINTS, and the prompt row is DURABLE before
    // anything the turn produces is served — otherwise a feed can paint an
    // answer above the question it answers. Held as an ORDERING rather than a
    // timing: the store is armed to fail its writes, so the prompt's ack cannot
    // land until the failure is released, and the first activity frame must
    // still arrive after it.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    // Every write is refused from here until it is released.
    shim.store?.failWrites("the store is holding every batch");

    const started = shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    // Nothing can be served while nothing is acked; releasing is what lets the
    // prompt row land, and only then may the turn's frames follow it.
    shim.store?.failWrites(null);
    await started;
    await untilTerminal(watch);

    const keys = writtenKeys(shim.store?.writes() ?? []);
    const promptAt = keys.indexOf("prompt:t1");
    const firstActivityAt = keys.findIndex((key) => key.startsWith("activity:"));
    expect(promptAt).toBeGreaterThanOrEqual(0);
    expect(firstActivityAt).toBeGreaterThan(promptAt);
    // And the SERVED order agrees with the written one: the prompt is the first
    // thing the tail carries.
    const servedKinds = watch
      .frames()
      .filter((frame) => frame.frame.case === "entry")
      .map((frame) => watchAgentEntry(frame).entry?.entry.case ?? "");
    expect(servedKinds[0]).toBe("userPrompt");
    watch.close();
  });
});

describe("RollBackSession", () => {
  /** Run one whole turn of `text` to its terminal. */
  async function wholeTurn(shim: Awaited<ReturnType<typeof spawnShim>>, turn: string, text: string): Promise<void> {
    const watch = openStream((options) => shim.clients.h1.watchAgent(watchAgentRequest(), options));
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text }));
    await untilTerminal(watch);
    watch.close();
  }

  const rollBack = rollBackSessionRequestFor;

  test("the vendor files each prompt under its turn id's derived uuid", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    await wholeTurn(shim, "t1", "!md");

    expect(userPrompts(readTranscript(shim.dirs, started.vendorSessionId)).map((record) => record.uuid)).toEqual([
      promptVendorUuid("t1"),
    ]);
  });

  test("a landed rollback resumes the conversation at the dropped prompt's parent", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await wholeTurn(shim, "t1", "!md");
    await wholeTurn(shim, "t2", "!md");
    const forkPoint = readTranscript(shim.dirs, started.vendorSessionId).find(
      (record) => record.uuid === promptVendorUuid("t2"),
    )?.parentUuid;

    const response = await shim.clients.h1.rollBackSession(rollBack("t2", ["t2"], "keep"));
    await wholeTurn(shim, "t3", "!md");

    const t3 = readTranscript(shim.dirs, started.vendorSessionId).find((record) => record.uuid === promptVendorUuid("t3"));
    expect({ result: response.result.case, parent: t3?.parentUuid }).toEqual({ result: "success", parent: forkPoint });
  });

  test("a prompt the caller did not name after the cut refuses unseen_prompt", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await wholeTurn(shim, "t1", "!md");
    await wholeTurn(shim, "t2", "!md");
    await wholeTurn(shim, "t3", "!md");

    const response = await shim.clients.h1.rollBackSession(rollBack("t2", ["t2"], "keep"));

    expect(
      response.result.case === "failure" && response.result.value.cause.case === "unseenPrompt"
        ? response.result.value.cause.value.vendorPromptUuid
        : response.result.case,
    ).toBe(promptVendorUuid("t3"));
  });

  test("a shim restarted before the next prompt resumes at the rollback's cut", async () => {
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await wholeTurn(first, "t1", "!md");
    await wholeTurn(first, "t2", "!md");
    const forkPoint = readTranscript(first.dirs, started.vendorSessionId).find(
      (record) => record.uuid === promptVendorUuid("t2"),
    )?.parentUuid;
    await first.clients.h1.rollBackSession(rollBack("t2", ["t2"], "keep"));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    sessionStarted(await second.clients.h1.startSession(resumeSession(started.vendorSessionId, undefined, ["t2"])));
    await wholeTurn(second, "t3", "!md");

    const t3 = readTranscript(first.dirs, started.vendorSessionId).find((record) => record.uuid === promptVendorUuid("t3"));
    expect(t3?.parentUuid).toBe(forkPoint);
  });

  test("files the vendor cannot restore refuse files_not_restorable", async () => {
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "rewind_files" } });
    await shim.clients.h1.startSession(freshSession());
    await wholeTurn(shim, "t1", "!md");
    await wholeTurn(shim, "t2", "!md");

    const response = await shim.clients.h1.rollBackSession(rollBack("t2", ["t2"], "restore"));

    expect(response.result.case === "failure" ? response.result.value.cause.case : response.result.case).toBe(
      "filesNotRestorable",
    );
  });
});
