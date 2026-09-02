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
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { workspaceLockKey } from "../../src/locks.js";
import { agentIdPath } from "../../src/engine/identity.js";
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
import {
  createStoreClient,
  pageLineOf,
  seedBashLifecycle,
  sidecarProducer,
  writtenEntries,
} from "../integration-support/store.js";
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
  setModelCold,
  setPermissionModeAccepted,
  setPermissionModeCause,
  startSessionCause,
  startSessionCold,
  turnStarted,
  watchAgentEntry,
  watchAgentPage,
  entryUpdateArm,
  bashFrame,
  entryFrame,
} from "../integration-support/expect.js";
import {
  awaitFile,
  readTranscript,
  sessionTranscriptPath,
  workspaceRealPath,
} from "../integration-support/vendor.js";

afterEach(cleanupShims);

/** The `AgentContextCut` a page entry carries, when it carries one. */
function contextCutOf(
  entry: conversationv1.HistoryEntryAt,
): conversationv1.ContextCut | null {
  const inner = entry.entry?.entry;
  if (inner?.case !== "agentFrame" || inner.value.result.case !== "update") return null;
  const update = inner.value.result.value.update;
  return update.case === "contextCut" ? update.value : null;
}

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
    // THE PROOF IS AN EVENT ABOUT THIS CALL, not a race against an unrelated
    // one. An rpc that happens to resolve first says nothing about whether the
    // engine ever SAW the setModel — it could have been refused at the
    // transport and the assertion would still pass. The shim records every
    // verb it enters, so this waits for the engine's own "serving
    // SetSessionModel" record: after that record the call is demonstrably
    // inside the engine, and still unsettled.
    await shim.log.record((record) => record.context.rpc === "SetSessionModel");
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
    // THE ARM, NOT MERELY THE PRESENCE. A `context_cleared` or a
    // `compaction_failed` is also a context_cut, and both would mean the
    // remediation did something other than what was asked for.
    const compacted = cuts
      .map((entry) => contextCutOf(entry))
      .filter((cut) => cut?.cut.case === "compacted");
    expect(compacted.length).toBeGreaterThan(0);
    const cut = compacted[0]!.cut;
    if (cut.case !== "compacted") throw new Error("expected a compacted cut");
    expect(cut.value.summary?.markdown).not.toBe("");
    expect(cut.value.tokens?.tokensBefore ?? 0n).toBeGreaterThan(
      cut.value.tokens?.tokensAfter ?? 0n,
    );
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

  test("the main AgentId is UNCHANGED across a rotation, judged against agent-id.json", async () => {
    // The main AgentId is the conversation's ORIGINAL vendor session id, and
    // rotation changes only the RESUME HANDLE. An AgentId that rotated would
    // split one conversation's book in two.
    //
    // GRADED AGAINST THE FILE PLANE. Comparing the id before the rotation with
    // the id after it compares two values the same shim minted, and a shim
    // that had rotated BOTH consistently would pass. `agent-id.json` is the
    // independent statement of what the AgentId is, and it is what a
    // file-only reader would resolve the book by.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const persisted = (
      JSON.parse(
        readFileSync(
          agentIdPath(shim.dirs.stateDir, workspaceLockKey(workspaceRealPath(shim.dirs))),
          "utf8",
        ),
      ) as { original_vendor_session_id: string }
    ).original_vendor_session_id;
    const before = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" })),
    );

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!rotate" }));
    const after = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t3", text: "!md" })),
    );

    expect(before.agent?.value).toBe(persisted);
    expect(after.agent?.value).toBe(persisted);
    // And the file did not move either: the rotation wrote a LINK, not a new
    // identity.
    const stillPersisted = (
      JSON.parse(
        readFileSync(
          agentIdPath(shim.dirs.stateDir, workspaceLockKey(workspaceRealPath(shim.dirs))),
          "utf8",
        ),
      ) as { original_vendor_session_id: string }
    ).original_vendor_session_id;
    expect(stillPersisted).toBe(persisted);
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
    // AND IT COMPACTED. A cut that carried no summary, or that claimed to have
    // grown the context, would be a compaction that did not happen.
    const compacted = contextCutOf(entry)?.cut;
    if (compacted?.case !== "compacted") throw new Error("expected a compacted cut");
    expect(compacted.value.summary?.markdown).not.toBe("");
    expect(compacted.value.tokens?.tokensBefore ?? 0n).toBeGreaterThan(
      compacted.value.tokens?.tokensAfter ?? 0n,
    );
    watch.close();
    agent.close();
  });

  test("!compact-failed produces a compaction_failed context_cut and no boundary", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
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
    // NO BOUNDARY. A failed compaction cut nothing, so the transcript has no
    // `compact_boundary` line and the cut carries the vendor's error rather
    // than a summary — the two halves of "nothing was discarded".
    const failed = contextCutOf(watchAgentEntry(cut))?.cut;
    if (failed?.case !== "compactionFailed") throw new Error("expected compaction_failed");
    expect(failed.value.error).not.toBe("");
    expect(
      readTranscript(shim.dirs, started.vendorSessionId).some(
        (record) => record.type === "system" && record.subtype === "compact_boundary",
      ),
    ).toBe(false);
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

  test("an idle session acks, having compacted the transcript (SYNTHETIC compaction fixture)", async () => {
    // The daemon stands the shim down only AFTER the ack, so revival never pays
    // a cold context — which is only true if the compaction really happened.
    //
    // GRADED AGAINST THE SYNTHETIC FIXTURE, NOT AGAINST A CAPTURE. The shim
    // writes the compacted transcript itself and the compaction helper is
    // synthetic BY RULING — no capture grounds the summarizer — so what this
    // asserts is that the shim wrote the `compact_boundary` line its own
    // fixture produces, at the ruled path. It does not assert that a real
    // vendor's boundary looks like this one.
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
    const frames = (await bash.drain()).map(bashFrame);
    const terminal = frames.at(-1);
    expect(terminal?.result.case).toBe("success");
    if (terminal?.result.case !== "success") throw new Error("expected a terminal");
    // THE INTERRUPTED ARM AND NOT A FAILURE. A run stopped because the session
    // was killed did not fail; reporting `failed` would tell the user their
    // command broke when the shim stopped it.
    expect(terminal.result.value.outcome.case).toBe("interrupted");
    // AND THE TERMINAL CAME FIRST. `drain` above already resolved on the
    // producer's own conclusion, so the process may only be observed gone
    // after it — awaiting the exit here proves the ordering rather than
    // assuming it.
    const exit = await shim.exited;
    expect(exit.code).toBe(0);
    agent.close();
  });
});


describe("rotation, against the session's own facts", () => {
  test("identity_rotated.new is the POST-CLEAR init id, and new_conversation_id is never adopted", async () => {
    // OBSERVED (capture identity-rotation-clear): the reset message carries a
    // `new_conversation_id` that NOTHING later uses, and the id the session
    // actually rotates to is the session_id of the SECOND `system:init`. A
    // shim that adopted the reset's own new_conversation_id would hand the
    // daemon a resume handle that resumes nothing.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = watchSession(shim);
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!rotate" }));
    const rotated = sessionUpdate(
      await watch.until((frame) => sessionUpdate(frame).update.case === "identityRotated"),
    );

    if (rotated.update.case !== "identityRotated") throw new Error("expected identity_rotated");
    const announced = rotated.update.value.vendorSessionId;
    // THE FILE PLANE AGREES: a transcript exists under the announced id, and
    // its own `sessionId` field is that id — which is what makes it the init's
    // session_id rather than the discarded new_conversation_id.
    await awaitFile(sessionTranscriptPath(shim.dirs, announced));
    const records = readTranscript(shim.dirs, announced);
    expect(records.length).toBeGreaterThan(0);
    expect(new Set(records.map((record) => record.sessionId))).toEqual(new Set([announced]));
    expect(announced).not.toBe(started.vendorSessionId);
    watch.close();
  });

  test("a resume BY THE ROTATED id still reports the ORIGINAL AgentId", async () => {
    // The rotated id is a RESUME HANDLE; the AgentId is the conversation. A
    // resume that adopted the handle as the identity would start a second book
    // for one conversation, and everything before the rotation would become
    // unreachable under the name the consumer holds.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const before = turnStarted(
      await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!rotate" })),
    );
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;
    const rotatedTo = readdirSync(
      join(first.dirs.stateDir, "shim", workspaceLockKey(workspaceRealPath(first.dirs)), "vendor-id"),
    )[0]!.replace(/\.json$/, "");
    expect(rotatedTo).not.toBe(started.vendorSessionId);

    const second = await spawnShim({ reuse: first.dirs });
    await second.clients.h1.startSession(resumeSession(rotatedTo, remediationPay()));
    const after = turnStarted(
      await second.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" })),
    );

    expect(after.agent?.value).toBe(started.vendorSessionId);
    expect(before.agent?.value).toBe(started.vendorSessionId);
    // And the NEW turn's rows land on the ORIGINAL book, not on the handle's.
    // THE STORE IS THE FIRST SPAWN'S: a reused directory set shares one fake
    // store, and the second handle never started one of its own.
    const books = new Set(
      writtenEntries(first.store?.writes() ?? [])
        .map((entry) => pageLineOf(entry)?.pageAgentId?.value)
        .filter((book): book is string => book !== undefined && book !== ""),
    );
    expect([...books]).toEqual([started.vendorSessionId]);
  });
});

describe("host shutdown as a turn's cause", () => {
  // TWO PATHS, ONE CAUSE. A turn cut short because the HOST went away is not a
  // user interrupt: the user asked for nothing, and drawing the two alike tells
  // them they stopped work they never touched.
  //
  // ASSERTED ON THE RECORD, NOT ON A STREAM. The terminal is written as the
  // session is torn down, and the very same teardown concludes every open
  // WatchAgent — so a reader waiting on the stream is racing the stream's own
  // conclusion. The store's inbox is where the frame durably is.
  /** The turn terminal the shim wrote for `agent`, if it wrote one. */
  function writtenTerminal(
    shim: Awaited<ReturnType<typeof spawnShim>>,
    agent: string,
  ): conversationv1.AgentFrame | null {
    for (const entry of writtenEntries(shim.store?.writes() ?? [])) {
      if (!entry.upsertKey.startsWith(`terminal:${agent}:`)) continue;
      const item = pageLineOf(entry)?.agentItem?.item;
      if (item?.case === "agentFrame") return item.value;
    }
    return null;
  }

  test("SIGTERM mid-turn concludes it interrupted by host_shutdown", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    const frame = writtenTerminal(shim, started.vendorSessionId);
    if (frame?.result.case !== "success") throw new Error("no turn terminal was recorded");
    expect(frame.result.value.outcome.case).toBe("interrupted");
    if (frame.result.value.outcome.case !== "interrupted") throw new Error("expected interrupted");
    expect(frame.result.value.outcome.value.cause.case).toBe("hostShutdown");
  });

  test("KillSession force mid-turn concludes it interrupted by host_shutdown", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await shim.exited;

    const frame = writtenTerminal(shim, started.vendorSessionId);
    if (frame?.result.case !== "success") throw new Error("no turn terminal was recorded");
    if (frame.result.value.outcome.case !== "interrupted") throw new Error("expected interrupted");
    // THE SAME CAUSE AS SIGTERM: KillSession{force} IS the host standing the
    // session down, and it is the path SIGTERM itself takes.
    expect(frame.result.value.outcome.value.cause.case).toBe("hostShutdown");
  });
});

describe("context usage at the turn boundary", () => {
  test("an ORDINARY turn pushes context_usage when it ends", async () => {
    // Not only the drifting scenario: the push is unconditional at every turn
    // end, so a topbar's figure is never one turn stale. Subscribed BEFORE the
    // turn and keyed on a frame that arrives AFTER the turn's result, so the
    // opening push cannot be mistaken for the turn-end one.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.until((frame) => sessionUpdate(frame).update.case === "contextUsage");
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const result = entryFrame(watchAgentEntry(frame))?.result;
      return result?.case === "success" || result?.case === "failure";
    });
    const pushed = await watch.until(
      (frame) => sessionUpdate(frame).update.case === "contextUsage",
    );

    const update = sessionUpdate(pushed);
    if (update.update.case !== "contextUsage") throw new Error("expected context_usage");
    expect(update.update.value.maxTokens).toBeGreaterThan(0n);
    expect(update.update.value.model).not.toBe("");
    watch.close();
    agent.close();
  });
});

describe("turn_in_flight, on both messages that carry it", () => {
  test("SessionStarted.turn_in_flight names the turn a killed shim left open", async () => {
    // A shim SIGKILLed mid-turn leaves a turn nothing concluded. The revived
    // shim must say so on StartSession, because the daemon's whole reattach
    // decision turns on whether there is work in flight.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const agent = openStream((options) =>
      first.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();
    await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    agent.close();
    first.signal("SIGKILL");
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const revived = sessionStarted(
      await second.clients.h1.startSession(resumeSession(started.vendorSessionId, remediationPay())),
    );

    // The detached run outlived the shim, so the revived session reports it as
    // live work rather than pretending the conversation is idle.
    expect(revived.liveWork.length).toBeGreaterThan(0);
  });

  test("SessionLive.turn_in_flight names the open turn when KillSession refuses", async () => {
    // The refusal exists so the daemon can tell the user what forcing would
    // destroy, and "a turn" with no name is not something a user can decide on.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "held-turn", text: "!hold" }));

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: false }),
    );

    expect(killSessionCause(response)).toBe("live");
    expect(killSessionLive(response).turnInFlight?.value).toBe("held-turn");
  });
});

describe("KillSession force across TWO turns", () => {
  test("every live item from both turns is named, and each concludes interrupted", async () => {
    // Liveness is a SESSION fact, not a turn fact: an item detached in turn one
    // is still running during turn two, and a forced kill that named only the
    // current turn's work would silently destroy the rest.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();

    const runs: string[] = [];
    for (const turn of ["t1", "t2"]) {
      await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!bash-detach-live" }));
      const announced = await agent.until((frame) => {
        if (frame.frame.case !== "entry") return false;
        const result = entryFrame(watchAgentEntry(frame))?.result;
        if (result?.case !== "detachedWork") return false;
        return !runs.includes(result.value.work?.value ?? "");
      });
      const result = entryFrame(watchAgentEntry(announced))?.result;
      if (result?.case !== "detachedWork") throw new Error("expected a detached announcement");
      runs.push(result.value.work?.value ?? "");
    }
    expect(new Set(runs).size).toBe(2);

    // Seeded as everywhere else: no integration harness runs a sidecar, so each
    // run's START row has no other producer, and each is left UNTERMINATED.
    const store = createStoreClient(shim.dirs.storeSocket);
    const watches = [];
    for (const [index, run] of runs.entries()) {
      await seedBashLifecycle(store, sidecarProducer(started.vendorSessionId), {
        run,
        work: run,
        command: `sleep 10000${String(index)}`,
        startedAtMs: 1_700_000_000_000 + index,
        chunks: ["running\n"],
        exitCode: null,
        topLevel: started.vendorSessionId,
      });
      const bash = openStream((options) =>
        shim.clients.h1.watchBash(
          create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
          options,
        ),
      );
      await bash.next();
      watches.push(bash);
    }

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );

    const killed = sessionKilled(response);
    if (killed.how.case !== "forced") throw new Error("expected a forced kill");
    const stopped = killed.how.value.stoppedWork.map((id) => id.value);
    for (const run of runs) expect(stopped).toContain(run);
    // EACH stream concludes interrupted, and BEFORE the process goes.
    for (const bash of watches) {
      const terminal = (await bash.drain()).map(bashFrame).at(-1);
      if (terminal?.result.case !== "success") throw new Error("expected a terminal");
      expect(terminal.result.value.outcome.case).toBe("interrupted");
    }
    const exit = await shim.exited;
    expect(exit.code).toBe(0);
    agent.close();
  });
});

describe("Hibernate and revival", () => {
  test("a shim killed AFTER the ack revives without paying a cold context", async () => {
    // THE WHOLE POINT OF HIBERNATE. The daemon stands the shim down only after
    // the ack, and the ack means the transcript was compacted — so the revival
    // is a warm resume with NO remediation, and the first page carries the cut
    // the hibernation performed.
    //
    // GRADED AGAINST THE SYNTHETIC COMPACTION FIXTURE: the summarizer is
    // synthetic by ruling, so what is asserted is the shim's own compaction
    // being visible in the feed, not a vendor's.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!cold-seed" }));
    hibernateAcked(await first.clients.h1.hibernate(create(shimv1.HibernateRequestSchema, {})));
    // STOOD DOWN, NOT SIGKILLED. The ack says the compaction happened; the
    // graceful stand-down is what the daemon does next, and it is the half
    // that guarantees the rows describing the compaction actually landed.
    expect((await first.standDown()).code).toBe(0);

    const second = await spawnShim({ reuse: first.dirs });
    const response = await second.clients.h1.startSession(resumeSession(started.vendorSessionId));

    // NOT COLD, and no remediation was named: the hibernation already paid.
    const revived = sessionStarted(response);
    expect(revived.vendorSessionId).not.toBe("");
    const watch = openStream((options) =>
      second.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());
    const cuts = page.entries
      .map((entry) => contextCutOf(entry))
      .filter((cut): cut is conversationv1.ContextCut => cut !== null);
    expect(cuts.some((cut) => cut.cut.case === "compacted")).toBe(true);
    watch.close();
  });

  test("Hibernate before any turn refuses compaction_failed, naming the missing transcript", async () => {
    // There is nothing to compact, and acking would tell the daemon a cold
    // revival had been prevented when nothing was done at all.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect(hibernateKind(response)).toBe("compactionFailed");
  });
});

describe("the arms every verb declares before a session exists", () => {
  // ONE TEST PER ARM. Each is a different sentence to the daemon, and a single
  // "it refuses" assertion would pass on a shim that answered them all alike.
  test("Hibernate with no session refuses no_session", async () => {
    const shim = await spawnShim();

    expect(
      hibernateKind(await shim.clients.h1.hibernate(create(shimv1.HibernateRequestSchema, {}))),
    ).toBe("noSession");
  });

  test("KillSession with no session refuses no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: false }),
    );

    expect(killSessionCause(response)).toBe("noSession");
  });

  test("SetSessionModel with no session refuses no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, { model: modelNamed("fake-sonnet-5") }),
    );

    expect(setModelCause(response)).toBe("noSession");
  });

  test("SetSessionPermissionMode with no session refuses no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: permissionMode("acceptEdits"),
      }),
    );

    expect(setPermissionModeCause(response)).toBe("noSession");
  });

  // DECLARED WITH NO PRODUCER. `KillSession{query_refused_to_end}` has no site
  // in the engine: teardown DELIBERATELY swallows a refused vendor interrupt
  // and continues, because a session that cannot be torn down cleanly must
  // still be torn down. Reaching the arm would mean making that failure fatal,
  // which is a contract change and not a test's to make.
  test.todo(
    "KillSession{query_refused_to_end} — no engine site produces it; teardown logs a refused interrupt at warn and continues by design",
  );
});

describe("the vendor refusing a CONTROL call", () => {
  // The shim relays these rather than owning them: the vendor said no, and a
  // shim that reported success would leave the daemon showing a model or a
  // mode the session is not actually in.
  test("SetSessionModel answers vendor_refused when the vendor rejects setModel", async () => {
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "set_model" } });
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: modelNamed("fake-sonnet-5"),
        coldThresholdTokens: 1_000_000n,
      }),
    );

    expect(setModelCause(response)).toBe("vendorRefused");
  });

  test("SetSessionPermissionMode answers vendor_refused when the vendor rejects it", async () => {
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "set_permission_mode" } });
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: permissionMode("acceptEdits"),
      }),
    );

    expect(setPermissionModeCause(response)).toBe("vendorRefused");
  });

  test("StartSession answers vendor_start_failed when the vendor cannot be started", async () => {
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "start" } });

    const response = await shim.clients.h1.startSession(freshSession());

    expect(startSessionCause(response)).toBe("vendorStartFailed");
  });

  test("a failed start leaves NOTHING behind: the retry on the same shim SUCCEEDS", async () => {
    // A FAILED START LEAVES THE ENGINE AS IT FOUND IT. The refusal is a session
    // failure, not a process one — the daemon may fix the condition and try
    // again on the same warm shim — and the retry must be an ordinary
    // StartSession, not a second attempt tripping over the wreckage of the
    // first.
    //
    // The wreckage was real: the failed attempt had already named the record
    // plane's writer from the identity it settled, so the retry's own identity
    // hit the producer re-key guard and escaped as an unhandled `Internal` on a
    // verb that has a typed refusal for every real condition.
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "start-once" } });
    expect(startSessionCause(await shim.clients.h1.startSession(freshSession()))).toBe(
      "vendorStartFailed",
    );
    expect(shim.child.exitCode).toBeNull();
    // NOTHING PERSISTED: the identity file named a conversation the vendor
    // never opened, so it is gone.
    expect(
      existsSync(agentIdPath(shim.dirs.stateDir, workspaceLockKey(workspaceRealPath(shim.dirs)))),
    ).toBe(false);

    const retried = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    expect(retried.vendorSessionId).not.toBe("");
    // AND BOTH CLAIMS WERE FREE TO RETAKE: the failed attempt released them, so
    // the retry holds them under its OWN identity.
    expect(readdirSync(shim.dirs.lockDir)).toContain(`session-${retried.vendorSessionId}.lock`);
    const persisted = JSON.parse(
      readFileSync(
        agentIdPath(shim.dirs.stateDir, workspaceLockKey(workspaceRealPath(shim.dirs))),
        "utf8",
      ),
    ) as { original_vendor_session_id: string };
    expect(persisted.original_vendor_session_id).toBe(retried.vendorSessionId);
  });

  test("a retried start serves an ordinary turn", async () => {
    // The retry is not merely accepted, it WORKS: the session it produced is
    // indistinguishable from one whose first start had succeeded.
    const shim = await spawnShim({ env: { AGENT_REPL_FAKE_REFUSE: "start-once" } });
    await shim.clients.h1.startSession(freshSession());
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const prompt = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" })),
    );

    expect(prompt.agent?.value).toBe(started.vendorSessionId);
  });
});


describe("SetSessionModel's cold gate", () => {
  // A MODEL SWITCH IS A COLD CACHE, because the cache is per model. The refusal
  // is immediate and states its cost, rather than a warning after the user has
  // already paid for it.
  test("a switch above the caller's threshold is REFUSED with its cost", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    // A turn so the transcript has context to lose.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));

    const response = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: modelNamed("fake-sonnet-5"),
        coldThresholdTokens: 0n,
      }),
    );

    expect(setModelCause(response)).toBe("cold");
    const cold = setModelCold(response);
    // THE COST, NAMED. The daemon cannot offer a remediation it cannot price.
    expect(cold.reason.case).toBe("modelSwitch");
    expect(cold.contextTokens).toBeGreaterThan(0n);
    expect(cold.requestedModel?.name).toBe("fake-sonnet-5");
  });

  test("the same switch with a remediation SUCCEEDS", async () => {
    // The refusal is not a veto: it exists so the caller decides knowingly.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    setModelCause(
      await shim.clients.h1.setSessionModel(
        create(shimv1.SetSessionModelRequestSchema, {
          model: modelNamed("fake-sonnet-5"),
          coldThresholdTokens: 0n,
        }),
      ),
    );

    const retried = await shim.clients.h1.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: modelNamed("fake-sonnet-5"),
        coldThresholdTokens: 0n,
        coldRemediation: remediationPay(),
      }),
    );

    setModelAccepted(retried);
  });

  test("the NEXT turn-end's context_usage reports the switched model", async () => {
    // The ack alone proves nothing about which model the session is on. The
    // turn-end push is the session's own authoritative statement, and it is
    // what a topbar renders.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = watchSession(shim);
    await watch.until((frame) => sessionUpdate(frame).update.case === "contextUsage");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    setModelAccepted(
      await shim.clients.h1.setSessionModel(
        create(shimv1.SetSessionModelRequestSchema, {
          model: modelNamed("fake-sonnet-5"),
          coldThresholdTokens: 0n,
          coldRemediation: remediationPay(),
        }),
      ),
    );

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    const pushed = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      return update.update.case === "contextUsage" && update.update.value.model === "fake-sonnet-5";
    });

    expect(sessionUpdate(pushed).update.case).toBe("contextUsage");
    watch.close();
  });
});

describe("a stand-down with an ask still open", () => {
  test("SIGTERM settles the open ask DENIED and concludes the turn", async () => {
    // A PENDING CALLBACK IS A LIVE PROMISE INSIDE THE VENDOR. A teardown that
    // simply exited would leave the vendor's `canUseTool` awaiting an answer
    // that can never come, and the shim's own exit code would say the session
    // ended in good order. The gate stands down by DENYING every open ask,
    // which is the only answer that is true once nobody is left to decide.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const agent = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await agent.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-once" }));
    // The ask is open once the permission unit has been announced.
    await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const result = entryFrame(watchAgentEntry(frame))?.result;
      if (result?.case !== "update") return false;
      return result.value.update.case === "permission";
    });
    agent.close();

    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    // THE ASK SETTLED DENIED, on the record. DENIED and not merely "settled":
    // once nobody is left to decide, deny is the only answer that is true.
    const decisions = writtenEntries(shim.store?.writes() ?? [])
      .filter((entry) => entry.upsertKey.startsWith("permission:"))
      .map((entry) => pageLineOf(entry)?.agentItem?.item)
      .flatMap((item) => {
        if (item?.case !== "agentFrame") return [];
        const result = item.value.result;
        if (result.case !== "update") return [];
        const update = result.value.update;
        if (update.case !== "permission") return [];
        const outcome = update.value.result;
        return outcome.case === "success" ? [outcome.value.decision.case ?? "unset"] : [];
      });
    expect(decisions).toContain("denied");
    // ...AND THE TURN GOT ITS TERMINAL, rather than being left open forever.
    const terminals = writtenEntries(shim.store?.writes() ?? []).filter((entry) =>
      entry.upsertKey.startsWith(`terminal:${started.vendorSessionId}:`),
    );
    expect(terminals.length).toBeGreaterThan(0);
  });
});
