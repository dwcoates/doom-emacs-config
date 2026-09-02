/**
 * test/integration/gate.test.ts — the ONE vendor gate, and its two meanings.
 *
 * `canUseTool` is the only gate the vendor has, and AskUserQuestion is a tool
 * riding it. MECHANISM SHARED, MEANING NOT: a question's "allow" is answer
 * TRANSPORT, a permission's allow IS CONSENT — two conversation.v1 units with
 * two identity spaces (a permission's id IS the gated call's activity id, so
 * consent joins to the work it gates; a question joins to no unit and has its
 * own id).
 *
 * The gate is also where liveness is most fragile: an unresolved `canUseTool`
 * promise wedges the vendor process, so every teardown path must resolve every
 * pending callback as denied before proceeding. The last test in this file is
 * that obligation.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  agentId,
  allowOnce,
  allowStanding,
  answerQuestion,
  denyPermission,
  freshSession,
  openStream,
  permissionMode,
  startTurnRequest,
  stopAgent,
  watchAgentRequest,
} from "../integration-support/client.js";
import {
  entryFrame,
  sessionUpdate,
  setPermissionModeAccepted,
  updateAccepted,
  updateAgentKind,
  watchAgentEntry,
} from "../integration-support/expect.js";

afterEach(cleanupShims);

type AgentStream = ReturnType<typeof openStream<shimv1.WatchAgentResponse>>;

/** Open the main agent's stream past its opening page. */
async function openAgentStream(
  shim: Awaited<ReturnType<typeof spawnShim>>,
): Promise<AgentStream> {
  const stream = openStream((options) =>
    shim.clients.h1.watchAgent(watchAgentRequest(), options),
  );
  await stream.next();
  return stream;
}

/** The `AgentUpdate` a tailed entry carries, or null when it carries none. */
function updateOf(frame: shimv1.WatchAgentResponse): conversationv1.AgentUpdate | null {
  if (frame.frame.case !== "entry") return null;
  const agentFrame = entryFrame(watchAgentEntry(frame));
  return agentFrame?.result.case === "update" ? agentFrame.result.value : null;
}

/** Wait for the permission ask and return it. */
async function awaitPermissionStart(
  stream: AgentStream,
): Promise<conversationv1.AgentPermission> {
  const frame = await stream.until((f) => {
    const update = updateOf(f);
    return update?.update.case === "permission" && update.update.value.result.case === "start";
  });
  const update = updateOf(frame);
  if (update?.update.case !== "permission") throw new Error("expected a permission frame");
  return update.update.value;
}

/** Wait for the question ask and return it. */
async function awaitQuestionStart(stream: AgentStream): Promise<conversationv1.AgentQuestion> {
  const frame = await stream.until((f) => {
    const update = updateOf(f);
    return update?.update.case === "question" && update.update.value.result.case === "start";
  });
  const update = updateOf(frame);
  if (update?.update.case !== "question") throw new Error("expected a question frame");
  return update.update.value;
}

/** Wait for the permission's own settled frame. */
async function awaitPermissionSettled(
  stream: AgentStream,
): Promise<conversationv1.AgentPermission> {
  const frame = await stream.until((f) => {
    const update = updateOf(f);
    return (
      update?.update.case === "permission" &&
      (update.update.value.result.case === "success" ||
        update.update.value.result.case === "failure")
    );
  });
  const update = updateOf(frame);
  if (update?.update.case !== "permission") throw new Error("expected a permission frame");
  return update.update.value;
}

describe("a permission ask", () => {
  test("the ask arrives as AgentPermission.start with the vendor's rendered prompt", async () => {
    // The VENDOR renders the prompt sentence (title/displayName/description);
    // the shim never composes one, because a client that had to compose it would
    // be inventing consent language.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    const ask = await awaitPermissionStart(stream);

    expect(ask.id?.value).not.toBe("");
    if (ask.result.case === "start") {
      expect(ask.result.value.prompt?.title).not.toBe("");
      expect(ask.result.value.prompt?.displayName).not.toBe("");
      expect(ask.result.value.startedAt?.atMs).toBeGreaterThan(0n);
    }
    stream.close();
  });

  test("the permission's id IS the gated call's activity id", async () => {
    // CONSENT JOINS TO THE WORK IT GATES. A permission id minted for the prompt
    // would leave the feed unable to attach the consent to the call.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    const ask = await awaitPermissionStart(stream);

    expect(ask.gatedCall?.value).toBe(ask.id?.value);
    stream.close();
  });

  test("the turn BLOCKS on the ask: no terminal arrives until it is answered", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    const ask = await awaitPermissionStart(stream);

    // A second StartTurn resolves NOW and is refused for the open turn — the
    // round trip proves the shim is responsive while the turn is blocked, with
    // no timer involved.
    const second = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );
    // THE BLOCK IS AN ORDERING, NOT A RACE. "No terminal yet" over the frames
    // pulled so far only says this test has not pulled one — it would pass on a
    // shim that terminated the turn before the ask. So the ask is ANSWERED, the
    // stream is driven to the terminal, and the assertion is made over the
    // COMPLETE served sequence: the terminal's index is after the answer's
    // settled frame, and no terminal appears anywhere before it.
    await shim.clients.h1.updateAgent(
      allowOnce(create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" })),
    );
    await stream.until((frame) => {
      const agentFrame =
        frame.frame.case === "entry" ? entryFrame(watchAgentEntry(frame)) : null;
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });
    const served = stream.frames();
    const settledAt = served.findIndex((frame) => {
      const update = updateOf(frame);
      return (
        update?.update.case === "permission" && update.update.value.result.case === "success"
      );
    });
    const terminalAt = served.findIndex((frame) => {
      const agentFrame =
        frame.frame.case === "entry" ? entryFrame(watchAgentEntry(frame)) : null;
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    expect(second.result.case).toBe("failure");
    expect(settledAt).toBeGreaterThanOrEqual(0);
    expect(terminalAt).toBeGreaterThan(settledAt);
    stream.close();
  });

  test("allow-once settles allowed.once and the tool then runs under the SAME id", async () => {
    // An allowed tool runs as an ORDINARY unit under the same identity: the
    // consent and the work are one thing in the feed.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    const ask = await awaitPermissionStart(stream);
    const gated = ask.gatedCall?.value ?? "";

    const response = await shim.clients.h1.updateAgent(
      allowOnce(create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" })),
    );
    const settled = await awaitPermissionSettled(stream);
    const ran = await stream.until((frame) => {
      const update = updateOf(frame);
      return (
        update?.update.case === "activity" && update.update.value.activityId?.value === gated
      );
    });

    updateAccepted(response);
    if (settled.result.case === "success") {
      expect(settled.result.value.decision.case).toBe("allowed");
      if (settled.result.value.decision.case === "allowed") {
        expect(settled.result.value.decision.value.scope.case).toBe("once");
      }
    }
    const update = updateOf(ran);
    expect(update?.update.case).toBe("activity");
    stream.close();
  });

  test("allow-standing echoes the OFFERED standing back", async () => {
    // The standing is a TYPED ECHO TOKEN: the shim validates the echo against
    // the pending callback it already holds, so no new state is needed and a
    // forged grant cannot get through.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!perm-allow-standing" }),
    );
    const ask = await awaitPermissionStart(stream);
    const offered =
      ask.result.case === "start" ? ask.result.value.offeredStanding : undefined;
    if (offered === undefined) throw new Error("the ask offered no standing to echo");

    const response = await shim.clients.h1.updateAgent(
      allowStanding(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        offered,
      ),
    );
    const settled = await awaitPermissionSettled(stream);

    updateAccepted(response);
    if (
      settled.result.case === "success" &&
      settled.result.value.decision.case === "allowed" &&
      settled.result.value.decision.value.scope.case === "standing"
    ) {
      expect(settled.result.value.decision.value.scope.value.standing?.changes).toEqual(
        offered.changes,
      );
    } else {
      throw new Error("the permission did not settle allowed.standing");
    }
    stream.close();
  });

  test("the OFFERED standing carrying set_mode is accepted and pushes permission_mode_changed", async () => {
    // A standing grant can change the session's mode — but only when the ask
    // OFFERED that change. `!perm-allow-standing-mode` is the one lever whose
    // offer carries `set_mode`, so this is the grounded path: the grant echoes
    // the offer verbatim, the gate accepts it, and the mode change is restated
    // AUTHORITATIVELY on WatchSession rather than left implicit in the grant
    // the daemon happens to have sent.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await session.next();
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!perm-allow-standing-mode" }),
    );
    const ask = await awaitPermissionStart(stream);
    const offered =
      ask.result.case === "start" ? ask.result.value.offeredStanding : undefined;
    if (offered === undefined) throw new Error("the ask offered no standing to echo");
    expect(
      offered.changes.some((change) => change.change.case === "setMode"),
    ).toBe(true);

    const response = await shim.clients.h1.updateAgent(
      allowStanding(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        offered,
      ),
    );
    const pushed = await session.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "permissionModeChanged" &&
        update.update.value.permissionMode?.mode.case === "acceptEdits"
      );
    });

    updateAccepted(response);
    expect(sessionUpdate(pushed).update.case).toBe("permissionModeChanged");
    stream.close();
    session.close();
  });

  test("a standing ALTERED from the offer is refused answer_mismatch", async () => {
    // ANSWER VALIDATION IS FREE, AND IT IS THE WHOLE PROTECTION HERE. The gate
    // holds the ask it offered while the vendor blocks, so a grant that adds a
    // change nobody offered is an answer to a question the shim is not holding.
    // Accepting it would let a caller install permission rules and move the
    // session's mode through a grant the vendor never proposed.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!perm-allow-standing" }),
    );
    const ask = await awaitPermissionStart(stream);
    const offered =
      ask.result.case === "start" ? ask.result.value.offeredStanding : undefined;
    if (offered === undefined) throw new Error("the ask offered no standing to echo");
    const altered = create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        ...offered.changes,
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.SESSION,
          change: {
            case: "setMode",
            value: create(conversationv1.AgentPermissionModeSetSchema, {
              mode: create(conversationv1.AgentPermissionModeSchema, {
                mode: {
                  case: "acceptEdits",
                  value: create(conversationv1.AgentPermissionModeAcceptEditsSchema, {}),
                },
              }),
            }),
          },
        }),
      ],
    });

    const response = await shim.clients.h1.updateAgent(
      allowStanding(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        altered,
      ),
    );

    expect(updateAgentKind(response)).toBe("answerMismatch");
    stream.close();
  });

  test("a standing grant on an ask that offered NONE is refused answer_mismatch", async () => {
    // `!perm-no-standing` is the vendor shape where no standing rule could be
    // written for the call, so the ask carries no `offered_standing` and the
    // only decision it can produce is a once-allow.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!perm-no-standing" }),
    );
    const ask = await awaitPermissionStart(stream);
    expect(
      ask.result.case === "start" ? ask.result.value.offeredStanding : undefined,
    ).toBeUndefined();

    const response = await shim.clients.h1.updateAgent(
      allowStanding(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        create(conversationv1.AgentPermissionStandingSchema, {
          changes: [
            create(conversationv1.AgentPermissionChangeSchema, {
              destination: conversationv1.AgentPermissionDestination.SESSION,
              change: {
                case: "setMode",
                value: create(conversationv1.AgentPermissionModeSetSchema, {
                  mode: create(conversationv1.AgentPermissionModeSchema, {
                    mode: {
                      case: "bypass",
                      value: create(conversationv1.AgentPermissionModeBypassSchema, {}),
                    },
                  }),
                }),
              },
            }),
          ],
        }),
      ),
    );

    expect(updateAgentKind(response)).toBe("answerMismatch");
    stream.close();
  });

  test("a user deny settles denied.user, and the gated unit goes start → failure with no content", async () => {
    // THE RULING (project lead, 2026-09-01, final): starts are NOT deferred —
    // the `tool_use` block is on the stream before `canUseTool` fires — so the
    // gated unit DOES start, and the denial RETIRES it: its `failure` arm
    // settles with content UNSET (the producer observed no error content) and
    // `settled_at` stamped, never a success and never an output. It is drawn
    // denied through the permission unit, whose id IS the gated unit's
    // AgentActivityId — which is why no `denied` cause exists on
    // AgentToolFailure.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-deny-user" }));
    const ask = await awaitPermissionStart(stream);
    const gated = ask.gatedCall?.value ?? "";

    await shim.clients.h1.updateAgent(
      denyPermission(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        "not this time",
      ),
    );
    const settled = await awaitPermissionSettled(stream);
    // Drive to the turn's terminal so every frame this turn will ever produce
    // has been seen before the negative assertion below.
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    if (
      settled.result.case === "success" &&
      settled.result.value.decision.case === "denied" &&
      settled.result.value.decision.value.by.case === "user"
    ) {
      expect(settled.result.value.decision.value.by.value.message).toBe("not this time");
    } else {
      throw new Error("the permission did not settle denied.user");
    }
    // The gated unit's OWN frames, in order: exactly a start and then the
    // denial's failure. Anything else — a success, an output, or no terminal at
    // all — is the defect this asserts against.
    const gatedUnit = stream
      .frames()
      .map(updateOf)
      .filter((update): update is conversationv1.AgentUpdate => update !== null)
      .filter(
        (update) =>
          update.update.case === "activity" && update.update.value.activityId?.value === gated,
      )
      .map((update) =>
        update.update.case === "activity" ? update.update.value.item : { case: undefined },
      );
    expect(gatedUnit.map((item) => item.case)).toEqual(["bash", "bash"]);
    const arms = gatedUnit.map((item) =>
      item.case === "bash" ? item.value.result.case : "not-bash",
    );
    expect(arms).toEqual(["start", "failure"]);
    const terminal = gatedUnit[1];
    if (terminal?.case !== "bash" || terminal.value.result.case !== "failure") {
      throw new Error("the gated unit did not settle its failure arm");
    }
    const error = terminal.value.result.value.error;
    // Content UNSET, settled_at stamped: the ruling's exact shape.
    expect(error?.content).toBeUndefined();
    expect(error?.settledAt).toBeDefined();
    stream.close();
  });

  test("!perm-deny-policy settles denied.policy with NO open ask", async () => {
    // The vendor's own system permission-denied message: a denial that never
    // reached canUseTool. The `denied.by_policy` emissions only exist when
    // settings are loaded, which is why settingSources is load-bearing.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-deny-policy" }));
    const settled = await awaitPermissionSettled(stream);

    if (
      settled.result.case === "success" &&
      settled.result.value.decision.case === "denied"
    ) {
      expect(settled.result.value.decision.value.by.case).toBe("policy");
    } else {
      throw new Error("the permission did not settle denied.policy");
    }
    // No ask was ever opened for it.
    expect(
      stream.frames().some((frame) => {
        const update = updateOf(frame);
        return (
          update?.update.case === "permission" && update.update.value.result.case === "start"
        );
      }),
    ).toBe(false);
    stream.close();
  });

  test("!perm-undecidable settles denied.undecidable", async () => {
    // A KNOWN-OPEN arm: `sdk.d.ts` declares no discriminator separating "nobody
    // could decide" from an ordinary policy deny, so the classifier's own
    // decision_reason_type is the closest producer there is.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-undecidable" }));
    const settled = await awaitPermissionSettled(stream);

    if (
      settled.result.case === "success" &&
      settled.result.value.decision.case === "denied"
    ) {
      expect(settled.result.value.decision.value.by.case).toBe("undecidable");
    } else {
      throw new Error("the permission did not settle denied.undecidable");
    }
    stream.close();
  });

  test("an answer naming the WRONG ask is refused answer_mismatch", async () => {
    // Answer validation is free: the shim already holds the pending callback
    // while the agent blocks, so the echo is checked against the ask in hand.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    await awaitPermissionStart(stream);

    const response = await shim.clients.h1.updateAgent(
      allowOnce(
        create(conversationv1.AgentPermissionIdSchema, { value: "an-id-nobody-asked-under" }),
      ),
    );

    expect(updateAgentKind(response)).toBe("answerMismatch");
    stream.close();
  });
});

describe("an ask raised by a SUBAGENT", () => {
  test("it is answered by naming the subagent as the target", async () => {
    // `!subagent-detached-live` leaves a detached agent running past the turn
    // and has it raise its OWN gated call, with the subagent's `agentID` on the
    // vendor callback. The answer names that agent as `target` — the consumer
    // is answering the agent it is watching, not the main thread — and the ask
    // is keyed by its own AgentPermissionId either way, which is why the echo
    // still validates.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!subagent-detached-live" }),
    );
    const announced = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") {
      throw new Error("expected the detached agent's announcement");
    }
    const subagent = detached.result.value.work?.value ?? "";
    const ask = await awaitPermissionStart(stream);

    const response = await shim.clients.h1.updateAgent(
      allowOnce(
        create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" }),
        agentId(subagent),
      ),
    );
    const settled = await awaitPermissionSettled(stream);

    updateAccepted(response);
    if (settled.result.case !== "success" || settled.result.value.decision.case !== "allowed") {
      throw new Error("the subagent's ask did not settle allowed");
    }
    expect(settled.result.value.decision.value.scope.case).toBe("once");
    stream.close();
  });
});

describe("the session's permission mode conditions the gate", () => {
  // THE VENDOR APPLIES THE MODE, NOT THE SHIM. `SetSessionPermissionMode`
  // relays it down and the vendor decides whether a gated call is asked about
  // at all, so what is under test here is that the relay REACHES the decision:
  // the same scenario asks, does not ask, or is refused, purely by mode.

  /** Drive a gated turn to its terminal and return every frame served. */
  const gatedTurn = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
    stream: AgentStream,
    turn: string,
  ): Promise<shimv1.WatchAgentResponse[]> => {
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!perm-allow-once" }));
    await stream.until((frame) => {
      const agentFrame =
        frame.frame.case === "entry" ? entryFrame(watchAgentEntry(frame)) : null;
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });
    return stream.frames();
  };

  test("bypass opens NO ask: the gated call runs without one", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    setPermissionModeAccepted(
      await shim.clients.h1.setSessionPermissionMode(
        create(shimv1.SetSessionPermissionModeRequestSchema, {
          permissionMode: permissionMode("bypass"),
        }),
      ),
    );

    // The turn runs to its terminal, so every frame it will ever produce has
    // been served before the negative below — this is an ordering over a
    // COMPLETE sequence, not a snapshot of what happened to arrive.
    const served = await gatedTurn(shim, stream, "t1");

    expect(
      served.some((frame) => updateOf(frame)?.update.case === "permission"),
    ).toBe(false);
    stream.close();
  });

  test("dont_ask settles denied.policy with no ask", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    setPermissionModeAccepted(
      await shim.clients.h1.setSessionPermissionMode(
        create(shimv1.SetSessionPermissionModeRequestSchema, {
          permissionMode: permissionMode("dontAsk"),
        }),
      ),
    );

    const served = await gatedTurn(shim, stream, "t1");

    const settled = served
      .map(updateOf)
      .filter((update): update is conversationv1.AgentUpdate => update !== null)
      .filter((update) => update.update.case === "permission")
      .map((update) =>
        update.update.case === "permission" ? update.update.value : undefined,
      );
    const decision =
      settled[0]?.result.case === "success" ? settled[0].result.value.decision : undefined;
    if (decision?.case !== "denied") {
      throw new Error("the mode-refused call did not settle a denial");
    }
    expect(decision.value.by.case).toBe("policy");
    // And it settled without ever opening one.
    expect(settled.some((permission) => permission?.result.case === "start")).toBe(false);
    stream.close();
  });

  test("an ask OPEN at the change keeps the mode it was raised under", async () => {
    // The mode is a property of the SESSION going forward, never a retroactive
    // rewrite of an ask already in flight: a consumer looking at an open ask
    // must not have it answered out from under them by a mode change.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    const ask = await awaitPermissionStart(stream);

    setPermissionModeAccepted(
      await shim.clients.h1.setSessionPermissionMode(
        create(shimv1.SetSessionPermissionModeRequestSchema, {
          permissionMode: permissionMode("bypass"),
        }),
      ),
    );
    // The ask is still the shim's to answer: a mode change that had swallowed
    // it would make this a no_open_ask.
    const response = await shim.clients.h1.updateAgent(
      allowOnce(create(conversationv1.AgentPermissionIdSchema, { value: ask.id?.value ?? "" })),
    );
    const settled = await awaitPermissionSettled(stream);

    updateAccepted(response);
    if (settled.result.case !== "success" || settled.result.value.decision.case !== "allowed") {
      throw new Error("the ask open at the mode change did not settle allowed");
    }
    expect(settled.result.value.decision.value.scope.case).toBe("once");
    stream.close();
  });
});

describe("a question ask", () => {
  test("!ask-single arrives as a single-select batch", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-single" }));
    const ask = await awaitQuestionStart(stream);

    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questions = ask.result.value.batch?.questions ?? [];
    expect(questions.length).toBe(1);
    expect(questions[0]?.choices.case).toBe("singleSelect");
    expect(ask.id?.value).not.toBe("");
    stream.close();
  });

  test("!ask-multi arrives as a TWO-question batch mixing both choice kinds", async () => {
    // The answer map is keyed by the question's own TEXT rather than by
    // position, which is exactly why a two-question batch is the shape that
    // catches a positional join.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-multi" }));
    const ask = await awaitQuestionStart(stream);

    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questions = ask.result.value.batch?.questions ?? [];
    expect(questions.length).toBe(2);
    expect(new Set(questions.map((question) => question.choices.case))).toEqual(
      new Set(["multiSelect", "singleSelect"]),
    );
    stream.close();
  });

  test("an answered single-select settles with the SAME selection", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-single" }));
    const ask = await awaitQuestionStart(stream);
    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const question = ask.result.value.batch?.questions[0];
    const questionText = question?.question?.text ?? "";
    const label =
      question?.choices.case === "singleSelect"
        ? (question.choices.value.options[0]?.label?.label ?? "")
        : "";

    const response = await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: ask.id?.value ?? "" }),
        [{ question: questionText, labels: [label] }],
      ),
    );
    const settled = await stream.until((frame) => {
      const update = updateOf(frame);
      return update?.update.case === "question" && update.update.value.result.case === "success";
    });

    updateAccepted(response);
    const update = updateOf(settled);
    if (update?.update.case !== "question" || update.update.value.result.case !== "success") {
      throw new Error("the question did not settle");
    }
    const outcome = update.update.value.result.value.outcome;
    expect(outcome.case).toBe("answered");
    if (outcome.case === "answered") {
      expect(outcome.value.answers[0]?.question?.text).toBe(questionText);
      expect(outcome.value.answers[0]?.chosen[0]?.label?.label).toBe(label);
    }
    stream.close();
  });

  test("a multi-select's labels round-trip as the vendor's comma-joined string", async () => {
    // The vendor keys answers by question TEXT and COMMA-JOINS multi-selects;
    // the shim undoes both at the boundary. The proof is the model's own view:
    // the scenario reports what it received, and the settled unit reports the
    // labels — the two must agree.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-multi" }));
    const ask = await awaitQuestionStart(stream);
    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questions = ask.result.value.batch?.questions ?? [];
    const selections = questions.map((question) => {
      const options =
        question.choices.case === "multiSelect" || question.choices.case === "singleSelect"
          ? question.choices.value.options
          : [];
      const labels =
        question.choices.case === "multiSelect"
          ? options.slice(0, 2).map((option) => option.label?.label ?? "")
          : [options[0]?.label?.label ?? ""];
      return { question: question.question?.text ?? "", labels };
    });

    await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: ask.id?.value ?? "" }),
        selections,
      ),
    );
    const settled = await stream.until((frame) => {
      const update = updateOf(frame);
      return update?.update.case === "question" && update.update.value.result.case === "success";
    });

    const update = updateOf(settled);
    if (update?.update.case !== "question" || update.update.value.result.case !== "success") {
      throw new Error("the question did not settle");
    }
    const outcome = update.update.value.result.value.outcome;
    if (outcome.case !== "answered") throw new Error("the question did not settle answered");
    const multi = outcome.value.answers.find((answer) => answer.chosen.length > 1);
    expect(multi?.chosen.map((choice) => choice.label?.label)).toEqual(
      selections.find((selection) => selection.labels.length > 1)?.labels,
    );
    stream.close();
  });

  test("!ask-free carries the typed free text as the residue", async () => {
    // FREE-TEXT RESIDUE: whatever remains of the joined answer string after
    // every validated label is removed IS the typed free text. That subtraction
    // is the producer definition, and the vendor gives nothing else to key on.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-free" }));
    const ask = await awaitQuestionStart(stream);
    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questionText = ask.result.value.batch?.questions[0]?.question?.text ?? "";

    await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: ask.id?.value ?? "" }),
        [{ question: questionText, labels: [], freeText: "something nobody offered" }],
      ),
    );
    const settled = await stream.until((frame) => {
      const update = updateOf(frame);
      return update?.update.case === "question" && update.update.value.result.case === "success";
    });

    const update = updateOf(settled);
    if (update?.update.case !== "question" || update.update.value.result.case !== "success") {
      throw new Error("the question did not settle");
    }
    const outcome = update.update.value.result.value.outcome;
    if (outcome.case !== "answered") throw new Error("the question did not settle answered");
    expect(outcome.value.answers[0]?.freeText?.text).toBe("something nobody offered");
    stream.close();
  });

  test("a label the batch never offered is refused answer_mismatch", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-single" }));
    const ask = await awaitQuestionStart(stream);
    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questionText = ask.result.value.batch?.questions[0]?.question?.text ?? "";

    const response = await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: ask.id?.value ?? "" }),
        [{ question: questionText, labels: ["a label nobody offered"] }],
      ),
    );

    expect(updateAgentKind(response)).toBe("answerMismatch");
    stream.close();
  });

  test("!ask-unanswered settles success.unanswered when the agent is STOPPED", async () => {
    // RULED: `sdk.d.ts` declares NO question timeout, so an expiry is modeled
    // as the gate's own DENY and the gap is recorded rather than invented --
    // which makes the STAND-DOWN the only producer of `unanswered`. Nothing
    // ever answers this question, so the test is the thing that ends it: the
    // stop resolves every pending callback as denied (an unresolved
    // `canUseTool` wedges the vendor), and the batch settles unanswered.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-unanswered" }));
    await awaitQuestionStart(stream);
    await shim.clients.h1.updateAgent(stopAgent());

    const settled = await stream.until((frame) => {
      const update = updateOf(frame);
      return update?.update.case === "question" && update.update.value.result.case === "success";
    });
    const update = updateOf(settled);
    if (update?.update.case !== "question" || update.update.value.result.case !== "success") {
      throw new Error("the question did not settle");
    }
    expect(update.update.value.result.value.outcome.case).toBe("unanswered");
    stream.close();
  });

  test("an answer with no open ask is refused no_open_ask", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: "nobody-asked" }),
        [{ question: "whatever", labels: ["yes"] }],
      ),
    );

    expect(updateAgentKind(response)).toBe("noOpenAsk");
  });

  test("a two-question batch answered OUT OF BATCH ORDER round-trips by question TEXT", async () => {
    // THE ANSWER MAP IS KEYED BY THE QUESTION'S OWN TEXT, never by position, so
    // an answer list in the reverse of the batch's order must join correctly.
    // A positional join would silently hand each question the other's chosen
    // labels — an answer the user never gave.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-multi" }));
    const ask = await awaitQuestionStart(stream);
    if (ask.result.case !== "start") throw new Error("expected the question's start arm");
    const questions = ask.result.value.batch?.questions ?? [];
    const inBatchOrder = questions.map((question) => {
      const options =
        question.choices.case === "multiSelect" || question.choices.case === "singleSelect"
          ? question.choices.value.options
          : [];
      return {
        question: question.question?.text ?? "",
        labels: [options[0]?.label?.label ?? ""],
      };
    });

    const response = await shim.clients.h1.updateAgent(
      answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: ask.id?.value ?? "" }),
        [...inBatchOrder].reverse(),
      ),
    );
    const settled = await stream.until((frame) => {
      const update = updateOf(frame);
      return update?.update.case === "question" && update.update.value.result.case === "success";
    });

    updateAccepted(response);
    const update = updateOf(settled);
    if (update?.update.case !== "question" || update.update.value.result.case !== "success") {
      throw new Error("the question did not settle");
    }
    const outcome = update.update.value.result.value.outcome;
    if (outcome.case !== "answered") throw new Error("the question did not settle answered");
    // Each question got ITS OWN label back, whatever order the answers arrived in.
    const byText = new Map(
      outcome.value.answers.map((answer) => [
        answer.question?.text ?? "",
        answer.chosen.map((choice) => choice.label?.label ?? ""),
      ]),
    );
    expect(inBatchOrder.map((selection) => byText.get(selection.question))).toEqual(
      inBatchOrder.map((selection) => selection.labels),
    );
    stream.close();
  });
});

describe("pending-callback liveness", () => {
  test("a forced KillSession during an open ask concludes it and the process exits cleanly", async () => {
    // AN UNRESOLVED canUseTool PROMISE WEDGES THE VENDOR PROCESS. Every teardown
    // path — interrupt, shutdown, SDK abort — resolves every pending callback as
    // denied before proceeding, and the exit is awaited on the process's own
    // exit event rather than on a clock.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    await awaitPermissionStart(stream);

    await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );
    const settled = await awaitPermissionSettled(stream);
    const exit = await shim.exited;

    if (settled.result.case === "success") {
      expect(settled.result.value.decision.case).toBe("denied");
    } else {
      throw new Error("the abandoned ask did not settle");
    }
    expect(exit.code).toBe(0);
    stream.close();
  });

  test("UpdateAgent.stop during an open ask denies it and interrupts the turn", async () => {
    // A STOP IS A TEARDOWN PATH LIKE ANY OTHER. The stop resolves every pending
    // callback as denied BEFORE it interrupts, because an unresolved
    // `canUseTool` promise survives the interrupt and wedges the vendor — the
    // turn would then never reach a terminal at all.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    // `!perm-hold` PARKS after the ask however it resolves, so the interrupt
    // is the only terminal it can reach — `!perm-allow-once` would race its own
    // recovery against the stop and sometimes conclude completed.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-hold" }));
    await awaitPermissionStart(stream);

    updateAccepted(await shim.clients.h1.updateAgent(stopAgent()));
    const settled = await awaitPermissionSettled(stream);
    const terminal = await stream.until((frame) => {
      const agentFrame =
        frame.frame.case === "entry" ? entryFrame(watchAgentEntry(frame)) : null;
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    if (settled.result.case !== "success" || settled.result.value.decision.case !== "denied") {
      throw new Error("the abandoned ask did not settle denied");
    }
    const frame = entryFrame(watchAgentEntry(terminal));
    if (frame?.result.case !== "success" || frame.result.value.outcome.case !== "interrupted") {
      throw new Error("the turn did not conclude AgentSuccess.interrupted");
    }
    expect(frame.result.value.outcome.value.cause.case).toBe("byUser");
    stream.close();
  });

  test("a query that DIES mid-ask denies the ask rather than leaving it open", async () => {
    // `!query-eof-mid-ask` opens the ask and then ends the iterable with the
    // callback still pending. The vendor is gone, so nothing will ever answer
    // it — and an ask left `start` forever is a feed showing a question that
    // can never be resolved.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!query-eof-mid-ask" }),
    );
    const settled = await awaitPermissionSettled(stream);

    if (settled.result.case !== "success") {
      throw new Error("the ask abandoned by a dead query did not settle");
    }
    expect(settled.result.value.decision.case).toBe("denied");
    stream.close();
  });

  test("SIGTERM during an open ask resolves it and stands down cleanly", async () => {
    // The daemon may already be dead, so the signal path cannot rely on the rpc
    // path having run first — it must resolve the callbacks itself.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-single" }));
    await awaitQuestionStart(stream);

    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    stream.close();
  });
});
