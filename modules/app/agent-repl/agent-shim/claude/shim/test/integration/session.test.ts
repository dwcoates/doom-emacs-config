/**
 * test/integration/session.test.ts — the SESSION verbs and WatchSession.
 *
 * StartSession/WatchSession/SetSessionModel/SetSessionPermissionMode/
 * Hibernate/KillSession, plus every `SessionUpdate` arm the mocked vendor can
 * produce. The through-line is the DECOUPLED-ACTS principle: a session outlives
 * the daemon, so its facts are reported authoritatively (even when the consumer
 * asked for the change) and its cold context is REFUSED with its cost rather
 * than silently paid.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, ITEST_BUILD_SHA, spawnShim } from "../integration-support/harness.js";
import {
  freshSession,
  openStream,
  permissionMode,
  remediationClear,
  remediationCompact,
  remediationPay,
  resumeSession,
  startTurnRequest,
  stopAgent,
  UNCATALOGED_MODEL,
  watchAgentRequest,
  workId,
  model as modelNamed,
  DEFAULT_MODEL,
} from "../integration-support/client.js";
import { createStoreClient, seedBashLifecycle, sidecarProducer } from "../integration-support/store.js";
import {
  hibernateAcked,
  hibernateKind,
  killSessionCause,
  killSessionLive,
  sessionKilled,
  sessionStarted,
  sessionUpdate,
  setModelAccepted,
  setModelCause,
  setPermissionModeAccepted,
  startSessionCause,
  startSessionCold,
  turnStarted,
  watchAgentEntry,
  watchAgentPage,
  entryUpdateArm,
  bashFrame,
} from "../integration-support/expect.js";
import {
  awaitFile,
  readTranscript,
  sessionTranscriptPath,
} from "../integration-support/vendor.js";

afterEach(cleanupShims);

/** Open the session watch and consume its opening frames up to `arm`. */
function watchSession(shim: Awaited<ReturnType<typeof spawnShim>>) {
  return openStream((options) =>
    shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
  );
}

describe("StartSession, fresh", () => {
  test("SessionStarted is fully populated", async () => {
    const shim = await spawnShim();

    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    expect(started.vendorSessionId).not.toBe("");
    expect(started.runtime?.shimBuildSha).toBe(ITEST_BUILD_SHA);
    expect(started.runtime?.sdkVersion).not.toBe("");
    expect(started.runtime?.agentBinaryVersion).not.toBe("");
    expect(started.effectiveModel?.name).not.toBe("");
    expect(started.permissionMode?.mode.case).toBe("default");
    expect(started.modelCatalog.length).toBeGreaterThan(0);
    // A fresh session has no turn and nothing live — expressed as absence and
    // an empty list, never as a sentinel.
    expect(started.turnInFlight).toBeUndefined();
    expect(started.liveWork).toEqual([]);
  });

  test("the vendor transcript materializes after the first turn", async () => {
    // The transcript is the one artifact nobody can regenerate, and it is the
    // sidecar's whole input, so its APPEARANCE at the ruled path is a contract.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const records = readTranscript(shim.dirs, started.vendorSessionId);
    expect(records.length).toBeGreaterThan(0);
  });

  test("a second StartSession is refused already_started", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const second = await shim.clients.h1.startSession(freshSession());

    expect(startSessionCause(second)).toBe("alreadyStarted");
  });
});

describe("WatchSession's opening frames", () => {
  test("the FIRST frame is diagnostics, immediately on open", async () => {
    // READINESS IS THE FIRST HEALTHY DIAGNOSTICS PUSH: connect-go surfaces a
    // server-stream refusal only at the first Receive, so a silent WatchSession
    // would block the daemon's bring-up.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = watchSession(shim);
    const first = await watch.next();

    expect(sessionUpdate(first).update.case).toBe("diagnostics");
    watch.close();
  });

  test("a SECOND concurrent subscriber also opens with diagnostics", async () => {
    // Every watch's opening frame is that watch's own answer; a fan-out that
    // only pushed to the first subscriber would wedge every later one.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = watchSession(shim);
    await first.next();

    const second = watchSession(shim);
    const opening = await second.next();

    expect(sessionUpdate(opening).update.case).toBe("diagnostics");
    first.close();
    second.close();
  });

  test("the opening frame reports healthy on a well-formed session", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = watchSession(shim);
    const first = sessionUpdate(await watch.next());

    expect(first.update.case).toBe("diagnostics");
    if (first.update.case === "diagnostics") {
      expect(first.update.value.health.case).toBe("healthy");
    }
    watch.close();
  });

  test("context_usage is pushed on open", async () => {
    // Sourced from the vendor's get_context_usage, never derived from usage
    // frames — and pushed at start so a topbar has a figure before any turn.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    const usage = await watch.until((frame) => sessionUpdate(frame).update.case === "contextUsage");

    const update = sessionUpdate(usage);
    expect(update.update.case).toBe("contextUsage");
    if (update.update.case === "contextUsage") {
      expect(update.update.value.maxTokens).toBeGreaterThan(0n);
    }
    watch.close();
  });

  test("model_changed is present on open", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    const changed = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "modelChanged",
    );

    expect(sessionUpdate(changed).update.case).toBe("modelChanged");
    watch.close();
  });

  test("permission_mode_changed is present on open", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    const changed = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "permissionModeChanged",
    );

    expect(sessionUpdate(changed).update.case).toBe("permissionModeChanged");
    watch.close();
  });
});

describe("SetSessionPermissionMode", () => {
  test("the new mode is pushed authoritatively on WatchSession", async () => {
    // AUTHORITATIVE EVEN WHEN THE CONSUMER ASKED: the daemon's own request is
    // not evidence the mode changed; the shim's push is.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.until((frame) => sessionUpdate(frame).update.case === "permissionModeChanged");

    const response = await shim.clients.h1.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: permissionMode("acceptEdits"),
      }),
    );
    const pushed = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "permissionModeChanged" &&
        update.update.value.permissionMode?.mode.case === "acceptEdits"
      );
    });

    setPermissionModeAccepted(response);
    expect(sessionUpdate(pushed).update.case).toBe("permissionModeChanged");
    watch.close();
  });
});

describe("SetSessionModel", () => {
  test("with no turn open it succeeds and pushes model_changed", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.until((frame) => sessionUpdate(frame).update.case === "modelChanged");

    const response = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, { model: modelNamed("fake-sonnet-5") }),
    );
    const pushed = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "modelChanged" &&
        update.update.value.effectiveModel?.name === "fake-sonnet-5"
      );
    });

    setModelAccepted(response);
    expect(sessionUpdate(pushed).update.case).toBe("modelChanged");
    watch.close();
  });

  test("a model the catalog does not carry is refused model_not_in_catalog", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, { model: modelNamed(UNCATALOGED_MODEL) }),
    );

    expect(setModelCause(response)).toBe("modelNotInCatalog");
  });

  test("during an open turn it resolves only after the turn ends", async () => {
    // ONE MODEL PER TURN, a deliberate departure from the SDK's mid-turn
    // setModel: a turn that changed model halfway would have two models'
    // reasoning in one answer.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    let settled = false;
    const pending = shim.clients.h1
      .setSessionModel(
        // The threshold is stated so the COLD GATE is not the subject here: a
        // turn has run, so the transcript now has context, and an unset
        // threshold (0) makes every switch a refused cold-cache switch before
        // the turn boundary is ever reached.
        create(shimv1.SetSessionModelRequestSchema, {
          model: modelNamed("fake-sonnet-5"),
          coldThresholdTokens: 1_000_000n,
        }),
      )
      .then((response) => {
        settled = true;
        return response;
      });
    // The turn is still open, so the call must not have resolved. Asserted by
    // racing it against an rpc that DOES resolve now — no timer involved.
    await shim.clients.h1.startSession(freshSession());
    expect(settled).toBe(false);

    // End the turn; only now may the model change land.
    await shim.clients.h1.updateAgent(stopAgent());
    setModelAccepted(await pending);
  });
});

describe("the cold gate", () => {
  test("a resume of a lapsed session is REFUSED with its cost", async () => {
    // A bare SDK resume makes no API call, so refusing costs nothing and the
    // cold read would land with the first prompt. The refusal carries the
    // evidence the daemon needs to offer a remediation.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!cold-seed" }));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const response = await second.clients.h1.startSession(
      resumeSession(started.vendorSessionId),
    );

    const cold = startSessionCold(response);
    expect(cold.reason.case).toBe("lapsed");
    expect(cold.contextTokens).toBeGreaterThan(0n);
    expect(cold.lastRequestAtMs).toBeGreaterThan(0n);
    expect(cold.requestedModel?.name).not.toBe("");
  });

  test("remediation.pay resumes and reports the recovered model and mode", async () => {
    // RESUME RECOVERS MODEL AND MODE FROM THE TRANSCRIPT: the SDK records them
    // there but does not restore them, so the shim reads the last of each back.
    const first = await spawnShim();
    const started = sessionStarted(
      await first.clients.h1.startSession(freshSession(DEFAULT_MODEL, "acceptEdits")),
    );
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!cold-seed" }));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const response = await second.clients.h1.startSession(
      resumeSession(started.vendorSessionId, remediationPay()),
    );

    const resumed = sessionStarted(response);
    expect(resumed.vendorSessionId).toBe(started.vendorSessionId);
    expect(resumed.effectiveModel?.name).not.toBe("");
    expect(resumed.permissionMode?.mode.case).toBe("acceptEdits");
  });

  test("remediation.clear resumes on a cleared context", async () => {
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!cold-seed" }));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const response = await second.clients.h1.startSession(
      resumeSession(started.vendorSessionId, remediationClear()),
    );

    expect(sessionStarted(response).vendorSessionId).not.toBe("");
  });

  test("remediation.compact resumes and the first page carries a compacted context_cut", async () => {
    // THE COMPACTION EXPERIMENT: the shim writes the summarized transcript, the
    // CLI resumes it, and the conversation serves. The cut is a page line, so
    // the compaction is visible in the feed rather than only in the record.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!cold-seed" }));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const response = await second.clients.h1.startSession(
      resumeSession(
        started.vendorSessionId,
        remediationCompact(DEFAULT_MODEL, conversationv1.SessionCompactScope.ALL),
      ),
    );
    sessionStarted(response);
    const watch = openStream((options) =>
      second.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());

    const cuts = page.entries.filter((entry) => entryUpdateArm(entry) === "contextCut");
    expect(cuts.length).toBeGreaterThan(0);
    watch.close();
  });

  test("a resume of an unknown vendor session id is refused unknown_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.startSession(
      resumeSession("00000000-0000-0000-0000-000000000000"),
    );

    expect(startSessionCause(response)).toBe("unknownSession");
  });
});

describe("identity rotation", () => {
  test("a /clear rotates the vendor id and pushes identity_rotated", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = watchSession(shim);
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!rotate" }));
    const rotated = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "identityRotated",
    );

    const update = sessionUpdate(rotated);
    expect(update.update.case).toBe("identityRotated");
    if (update.update.case === "identityRotated") {
      expect(update.update.value.previousVendorSessionId).toBe(started.vendorSessionId);
      expect(update.update.value.vendorSessionId).not.toBe(started.vendorSessionId);
    }
    watch.close();
  });

  test("the main AgentId is UNCHANGED across a rotation", async () => {
    // The main AgentId is the conversation's ORIGINAL vendor session id, and
    // rotation changes only the RESUME HANDLE. An AgentId that rotated would
    // split one conversation's book in two.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const before = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" })),
    );

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!rotate" }));
    const after = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t3", text: "!md" })),
    );

    expect(after.agent?.value).toBe(before.agent?.value);
  });
});

describe("session facts with no message behind them", () => {
  test("!rate-limit produces a rate_limit_status arm with its typed status", async () => {
    // LANDING 4: the vendor's `rate_limit_event` is its OWN arm now, not an
    // account-usage sample. The two say different things — one is the window's
    // verdict on this request, the other is the account's standing utilization —
    // and folding the event into account_usage lost the verdict.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!rate-limit" }));
    const frame = await watch.until(
      (f) => sessionUpdate(f).update.case === "rateLimitStatus",
    );

    const update = sessionUpdate(frame);
    if (update.update.case !== "rateLimitStatus") throw new Error("expected rate_limit_status");
    // The corpus shape: allowed_warning on the overage window with a threshold.
    expect(update.update.value.status.case).toBe("allowedWarning");
    expect(update.update.value.resetsAtMs).toBeDefined();
    expect(update.update.value.utilizationPercent).toBeDefined();
    expect(update.update.value.rateLimitType?.window.case).not.toBeUndefined();
    watch.close();
  });

  test("the rate-limit event's threshold rides surpassed_threshold_percent", async () => {
    // The threshold is the whole point of an `allowed_warning`: without it the
    // warning states that something was surpassed and never says what.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!rate-limit" }));
    const frame = await watch.until(
      (f) => sessionUpdate(f).update.case === "rateLimitStatus",
    );

    const update = sessionUpdate(frame);
    if (update.update.case !== "rateLimitStatus") throw new Error("expected rate_limit_status");
    expect(update.update.value.surpassedThresholdPercent).toBeDefined();
    watch.close();
  });

  test("!usage-full reports account usage AVAILABLE with its windows", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!usage-full" }));
    const frame = await watch.until((f) => {
      const update = sessionUpdate(f);
      return (
        update.update.case === "accountUsage" && update.update.value.outcome.case === "available"
      );
    });

    const update = sessionUpdate(frame);
    if (update.update.case === "accountUsage" && update.update.value.outcome.case === "available") {
      expect(update.update.value.outcome.value.fiveHour).toBeDefined();
    }
    watch.close();
  });

  // The four unavailable shapes are DISTINCT on the wire (rate_limits null, a
  // null WINDOW, a null utilization inside a present window, behaviors null),
  // so each is its own test rather than one "unavailable" assertion.
  for (const [scenario, reason] of [
    ["!usage-service-unavailable", "serviceUnavailable"],
    ["!usage-window-unavailable", "windowUnavailable"],
    ["!usage-utilization-unavailable", "utilizationUnavailable"],
  ] as const) {
    test(`${scenario} reports account usage unavailable as ${reason}`, async () => {
      const shim = await spawnShim();
      await shim.clients.h1.startSession(freshSession());
      const watch = watchSession(shim);

      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: scenario }));
      const frame = await watch.until((f) => {
        const update = sessionUpdate(f);
        return (
          update.update.case === "accountUsage" &&
          update.update.value.outcome.case === "unavailable" &&
          update.update.value.outcome.value.reason.case === reason
        );
      });

      expect(sessionUpdate(frame).update.case).toBe("accountUsage");
      watch.close();
    });
  }

  for (const [scenario, arm] of [
    ["!fast-on", "on"],
    ["!fast-off", "off"],
    ["!fast-cooldown", "cooldown"],
  ] as const) {
    test(`${scenario} produces fast_mode.${arm}`, async () => {
      const shim = await spawnShim();
      await shim.clients.h1.startSession(freshSession());
      const watch = watchSession(shim);

      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: scenario }));
      const frame = await watch.until((f) => {
        const update = sessionUpdate(f);
        return update.update.case === "fastMode" && update.update.value.state.case === arm;
      });

      expect(sessionUpdate(frame).update.case).toBe("fastMode");
      watch.close();
    });
  }

  test("!mcp-all produces one mcp_server arm per declared health", async () => {
    // The five healths are five arms, and a catalog that agreed about health
    // could not exercise a renderer that branches on them.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    const healths = new Set<string>();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!mcp-all" }));
    await watch.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case === "mcpServer") {
        healths.add(update.update.value.health.case ?? "unset");
      }
      return healths.size >= 5;
    });

    expect([...healths].sort()).toEqual(
      ["connected", "disabled", "failed", "needsAuth", "pending"].sort(),
    );
    watch.close();
  });

  // RETIRED AT LANDING 4 (tag 24): the budget warning is no longer a
  // SessionUpdate arm. It is a transcript ATTACHMENT the SIDECAR reads, served
  // as the page line `AgentUpdate.context_budget_warning{text}`, and the shim
  // never emits it live — so the WatchSession coverage becomes a NEGATIVE one,
  // and the positive coverage belongs to whoever tests the file plane.
  test.todo(
    "the budget warning appears as AgentUpdate.context_budget_warning on the SIDECAR's page line — the shim has no live producer, so this belongs to the file-plane suite",
  );

  test("!context-tip pushes NO session-level budget arm", async () => {
    // A shim still emitting the retired tag 24 would push a frame whose oneof
    // case the generated code cannot name, so an unrecognized arm is exactly
    // the failure this asserts against.
    const known = new Set([
      "identityRotated",
      "queryDied",
      "modelChanged",
      "fastMode",
      "mcpServer",
      "accountUsage",
      "permissionModeChanged",
      "rateLimitStatus",
      "diagnostics",
      "contextUsage",
      "compacting",
    ]);
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!context-tip" }));
    // Drive to the turn's terminal so every frame this turn produces has been
    // seen before the negative assertion below.
    await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const inner = watchAgentEntry(frame).entry?.entry;
      if (inner?.case !== "agentFrame") return false;
      return inner.value.result.case === "success" || inner.value.result.case === "failure";
    });

    const arms = watch.frames().map((frame) => sessionUpdate(frame).update.case ?? "unset");
    expect(arms.filter((arm) => !known.has(arm))).toEqual([]);
    watch.close();
    agent.close();
  });

  test("!context-usage-drift pushes a NEW context_usage at the turn end", async () => {
    // Context usage is pushed at every turn end, so a drifting figure must
    // reach the consumer without the consumer asking.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    const opening = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "contextUsage",
    );
    const openingUsage = sessionUpdate(opening);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!context-usage-drift" }));
    const later = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "contextUsage" &&
        openingUsage.update.case === "contextUsage" &&
        update.update.value.totalTokens !== openingUsage.update.value.totalTokens
      );
    });

    expect(sessionUpdate(later).update.case).toBe("contextUsage");
    watch.close();
  });

  test("!model-fallback pushes model_changed for a model the shim did not ask for", async () => {
    // The vendor can serve a different model than the one requested; the change
    // is stated from the next assistant message's own `message.model`.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.until((frame) => sessionUpdate(frame).update.case === "modelChanged");

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!model-fallback" }));
    const changed = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "modelChanged" &&
        update.update.value.effectiveModel?.name !== DEFAULT_MODEL
      );
    });

    expect(sessionUpdate(changed).update.case).toBe("modelChanged");
    watch.close();
  });
});

describe("compaction, as the vendor does it", () => {
  test("!compact-auto produces compacting and then a compacted context_cut", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!compact-auto" }));
    const compacting = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "compacting",
    );
    const cut = await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryUpdateArm(watchAgentEntry(frame)) === "contextCut";
    });

    expect(sessionUpdate(compacting).update.case).toBe("compacting");
    const entry = watchAgentEntry(cut);
    expect(entryUpdateArm(entry)).toBe("contextCut");
    watch.close();
    agent.close();
  });

  test("!compact-failed produces a compaction_failed context_cut and no boundary", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!compact-failed" }));
    const cut = await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const inner = watchAgentEntry(frame).entry?.entry;
      if (inner?.case !== "agentFrame" || inner.value.result.case !== "update") return false;
      const update = inner.value.result.value.update;
      return update.case === "contextCut" && update.value.cut.case === "compactionFailed";
    });

    expect(entryUpdateArm(watchAgentEntry(cut))).toBe("contextCut");
    agent.close();
  });
});

describe("query death", () => {
  test("!query-eof reports query_died.unexpected_eof", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!query-eof" }));
    const died = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "queryDied" && update.update.value.cause.case === "unexpectedEof"
      );
    });

    expect(sessionUpdate(died).update.case).toBe("queryDied");
    watch.close();
  });

  test("!query-fail reports query_died.iterator_failure", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!query-fail" }));
    const died = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "queryDied" && update.update.value.cause.case === "iteratorFailure"
      );
    });

    expect(sessionUpdate(died).update.case).toBe("queryDied");
    watch.close();
  });

  test("a query death concludes the open WatchAgent turn with a failure terminal", async () => {
    // DUPLICATED ON PURPOSE: stream owners get their own failure, and a
    // consumer with no stream open still needs the session-level fact.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!query-eof" }));
    const terminal = await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const inner = watchAgentEntry(frame).entry?.entry;
      return inner?.case === "agentFrame" && inner.value.result.case === "failure";
    });

    const inner = watchAgentEntry(terminal).entry?.entry;
    expect(inner?.case).toBe("agentFrame");
    agent.close();
  });
});

describe("the shim's own faults", () => {
  test("!fault-converter surfaces a converter_defect fault on diagnostics", async () => {
    // A converter that could not convert is a PRODUCER DEFECT, reported rather
    // than swallowed: the alternative is silently wrong attribution.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!fault-converter" }));
    const faulted = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "converterDefect")
      );
    });

    expect(sessionUpdate(faulted).update.case).toBe("diagnostics");
    watch.close();
  });

  test("a recovered fault closes its degraded window", async () => {
    // Diagnostics keep degraded WINDOWS since shim start, so a transient defect
    // stays visible after recovery instead of vanishing.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!fault-converter" }));
    await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "diagnostics" && update.update.value.health.case === "unhealthy"
      );
    });

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    const recovered = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return (
        update.update.case === "diagnostics" &&
        update.update.value.health.case === "healthy" &&
        update.update.value.degradedWindows.some((window) => window.extent.case === "closed")
      );
    });

    expect(sessionUpdate(recovered).update.case).toBe("diagnostics");
    watch.close();
  });
});

describe("Hibernate", () => {
  test("a turn in flight refuses turn_in_flight", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect(hibernateKind(response)).toBe("turnInFlight");
  });

  test("an idle session acks, having compacted the transcript", async () => {
    // The daemon stands the shim down only AFTER the ack, so revival never pays
    // a cold context — which is only true if the compaction really happened.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));

    const response = await shim.clients.h1.hibernate(create(shimv1.HibernateRequestSchema, {}));

    hibernateAcked(response);
    const records = readTranscript(shim.dirs, started.vendorSessionId);
    expect(
      records.some(
        (record) => record.type === "system" && record.subtype === "compact_boundary",
      ),
    ).toBe(true);
  });
});

describe("KillSession", () => {
  test("an idle session ends idle and the process exits 0", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: false }),
    );
    const exit = await shim.exited;

    expect(sessionKilled(response).how.case).toBe("idle");
    expect(exit.code).toBe(0);
  });

  test("live detached work refuses, NAMING the work", async () => {
    // Both outcomes name the work so the daemon can tell the user what forcing
    // would destroy.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const inner = watchAgentEntry(frame).entry?.entry;
      return inner?.case === "agentFrame" && inner.value.result.case === "detachedWork";
    });
    const inner = watchAgentEntry(announced).entry?.entry;
    const work =
      inner?.case === "agentFrame" && inner.value.result.case === "detachedWork"
        ? inner.value.result.value.work?.value
        : undefined;

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: false }),
    );

    expect(killSessionCause(response)).toBe("live");
    expect(killSessionLive(response).liveWork.map((id) => id.value)).toContain(work);
    agent.close();
  });

  test("force ends it, naming the stopped work", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: false }));

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );

    const killed = sessionKilled(response);
    expect(killed.how.case).toBe("forced");
    if (killed.how.case === "forced") {
      expect(killed.how.value.stoppedWork.length).toBeGreaterThan(0);
    }
  });

  test("a forced kill concludes the detached stream with an interrupted arm FIRST", async () => {
    // The stream's terminal frame is the consumer's only stop notice, and it
    // must arrive before the process goes: a stream that ended without one is
    // read as a transport failure.
    //
    // THE DETACHED STREAM IS `WatchBash`, not the agent's. A shell run's
    // lifecycle frames are lifecycle rows and never page lines, so its
    // interrupted terminal cannot appear on a WatchAgent tail — waiting for it
    // there waits for the process to die, which is the transport failure this
    // test exists to forbid.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const inner = watchAgentEntry(frame).entry?.entry;
      return inner?.case === "agentFrame" && inner.value.result.case === "detachedWork";
    });
    const inner = watchAgentEntry(announced).entry?.entry;
    const run =
      inner?.case === "agentFrame" && inner.value.result.case === "detachedWork"
        ? (inner.value.result.value.work?.value ?? "")
        : "";
    expect(run).not.toBe("");
    // Seeded as everywhere else: no integration harness runs a sidecar, so the
    // run's START row has no other producer. It is left UNTERMINATED, which is
    // what leaves the kill something live to conclude.
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "sleep 100000",
        startedAtMs: 1_700_000_000_000,
        chunks: ["running\n"],
        exitCode: null,
        topLevel: started.vendorSessionId,
      },
    );
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    await bash.next();

    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    // THE TERMINAL, AND NOT A CUT STREAM. `drain` resolves only on the
    // producer's own conclusion; the process going first would reject here.
    const terminal = (await bash.drain()).map(bashFrame).at(-1);
    expect(terminal?.result.case).toBe("success");
    if (terminal?.result.case === "success") {
      expect(terminal.result.value.outcome.case).toBe("interrupted");
    }
    agent.close();
  });
});
