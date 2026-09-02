/**
 * fake/scenarios/session.ts — the facts a session states about ITSELF.
 *
 * # Most SessionUpdate arms are ANSWERS, not messages
 *
 * `sdk.d.ts` declares no `model_changed`, no `permission_mode_changed`, no
 * `fast_mode`, no `mcp_server`, no `account_usage` and no
 * `context_budget_warning` system message. Those arms are produced by the shim
 * from CONTROL ANSWERS (`mcpServerStatus`, the account-usage probe,
 * `getContextUsage`) and from fields that ride other messages
 * (`status.permissionMode`, `result.fast_mode_state`, `init.fast_mode_state`).
 *
 * So the scenarios here do two different things. Where a message exists, they
 * emit it. Where only an answer exists, they FLIP THE ANSWER the mock will give
 * (`setAccountUsageArm`, `setMcpArm`) and let the shim's own cadence discover
 * it — which is the real production path, not a shortcut around it.
 */
import type { AccountUsageArm } from "../scenario.js";
import { conclude, scenario } from "./support.js";

export const ROTATE = scenario({
  name: "rotate",
  prompt: "!rotate",
  emits:
    "a `/clear`: the retired identity's transcript gets a closing record, `conversation_reset` announces the new " +
    "id, a fresh `system:init` follows, and the REST of the turn — its result included — belongs to the new identity",
  writes:
    "a closing system record on the OLD `<old-session>.jsonl` (left otherwise intact), then a NEW " +
    "`<new-session>.jsonl` carrying everything after the reset",
  arms: "SessionIdentityRotated + AgentUpdate.context_cut(ContextCleared)",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "rotate" }, "fake identity-rotation turn");
    const next = ctx.rotate();
    ctx.log({ new_claude_session_id: next }, "fake vendor minted a new conversation identity");
    conclude(ctx, "Cleared the conversation.");
  },
});

export const SLASH_LOCAL = scenario({
  name: "slash",
  prompt: "!slash",
  emits:
    "a slash command the VENDOR answers itself: a `local_command_output` message, and the transcript's " +
    "`system:local_command` record wrapping the output in `<local-command-stdout>`",
  writes: "a `system:local_command` line and a `command_permissions` attachment line",
  arms: "the vendor-answered slash-command family — no agent activity at all",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "slash" }, "fake vendor-answered slash-command turn");
    const output = "Session: offline\nModel: fake-opus-4-8\nPermission mode: default";
    ctx.systemMessage("local_command_output", { content: output });
    ctx.files.transcript.append({
      type: "system",
      subtype: "local_command",
      content: `<local-command-stdout>${output}</local-command-stdout>`,
      level: "info",
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.attachment({ type: "command_permissions", allowedTools: [] });
    conclude(ctx, "Answered the slash command locally.");
  },
});

export const CONTEXT_USAGE_DRIFT = scenario({
  name: "context-usage-drift",
  prompt: "!context-usage-drift",
  emits:
    "prose only. It switches `getContextUsage()` to a GROWING answer, so the `context_usage` the shim pushes at " +
    "this turn's end differs from the one it pushed at session start — total tokens, percentage, the message " +
    "category and the whole `messageBreakdown` all move, and the answer stays a full " +
    "`SDKControlGetContextUsageResponse`. CADENCE IS THE ENGINE'S: context_usage is pushed at session start and " +
    "at EVERY turn end regardless of scenario, so this one changes what is sampled and never when",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionContextUsage — the same arm twice with DIFFERENT figures, which is what a re-render tests",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "context-usage-drift" }, "fake context-usage drift turn");
    ctx.setContextUsageDrift(true);
    conclude(ctx, "The context-usage answer now grows with every turn.");
  },
});

export const MODEL_FALLBACK = scenario({
  name: "model-fallback",
  prompt: "!model-fallback",
  emits:
    "an UNSOLICITED model change: the declared `model_refusal_fallback` message with `direction: \"retry\"`, the " +
    "`session_state_changed` beat, and then the answer from the FALLBACK model — whose `message.model` is the only " +
    "evidence the swap happened. The swap STICKS, so a following turn answers on the fallback model too. Nothing " +
    "called SetSessionModel, so no confirmation exists anywhere",
  writes: "a `system:model_refusal_fallback` line, the fallback-model assistant line, the prompt line and the turn record",
  arms: "SessionModelChanged with no SetSessionModel behind it — the vendor's own decision, not a confirmed request",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "model-fallback" }, "fake unsolicited model-fallback turn");
    const original = ctx.model;
    const fallback = original === "fake-sonnet-5" ? "fake-haiku-4-5" : "fake-sonnet-5";
    const content = `Switched to ${fallback} for the rest of this session.`;
    ctx.emit({
      type: "system",
      subtype: "model_refusal_fallback",
      trigger: "refusal",
      direction: "retry",
      original_model: original,
      fallback_model: fallback,
      request_id: `req_fake_fallback_${String(ctx.turn)}`,
      api_refusal_category: "cyber",
      api_refusal_explanation: null,
      // EMPTY, and not omitted: the refusal landed before any block had been
      // delivered, so there is nothing for the consumer to evict. An absent list
      // would mean "an older CLI that cannot tell you", which is a different
      // fact.
      retracted_message_uuids: [],
      refused_user_message_uuid: null,
      content,
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "model_refusal_fallback",
      direction: "retry",
      content,
      level: "warning",
      trigger: "refusal",
      originalModel: original,
      fallbackModel: fallback,
      requestId: `req_fake_fallback_${String(ctx.turn)}`,
      apiRefusalCategory: "cyber",
      apiRefusalExplanation: null,
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.fallbackTo(fallback);
    // The declared beat a model switch produces. It carries NO model, which is
    // exactly why the assistant message below is the evidence.
    ctx.systemMessage("session_state_changed", { state: "idle" });
    conclude(ctx, `Answered on ${fallback} after the fallback.`);
  },
});

/** One fast-mode scenario per declared state. */
function fastModeScenario(name: string, state: "on" | "off" | "cooldown", reason?: string) {
  return scenario({
    name,
    prompt: `!${name}`,
    emits:
      `a turn whose \`result\` reports \`fast_mode_state: "${state}"\`` +
      `${reason === undefined ? "" : ` with \`fast_mode_disabled_reason: "${reason}"\``}` +
      ". The state STICKS: every later result reports it, and so does the `init` a rotation emits — the two places " +
      "`sdk.d.ts` carries fast mode at all",
    writes: "the assistant line, the prompt line and the turn record",
    arms: `SessionFastMode.state=${state}`,
    run(ctx) {
      ctx.log({ turn: ctx.turn, branch: name, fast_mode_state: state }, "fake fast-mode turn");
      ctx.setFastMode(state, reason);
      const conclusion = `Fast mode is ${state}.`;
      ctx.assistant([{ type: "text", text: conclusion }], { stopReason: "end_turn" });
      ctx.result({
        subtype: "success",
        result: conclusion,
        fastModeState: state,
        ...(reason === undefined ? {} : { fastModeDisabledReason: reason }),
      });
    },
  });
}

export const FAST_ON = fastModeScenario("fast-on", "on");
export const FAST_OFF = fastModeScenario("fast-off", "off", "preference");
export const FAST_COOLDOWN = fastModeScenario("fast-cooldown", "cooldown", "extra_usage_disabled");

export const MCP_ALL = scenario({
  name: "mcp-all",
  prompt: "!mcp-all",
  emits:
    "prose only. It switches `mcpServerStatus()` to the FIVE-server catalog — one per declared health: " +
    "connected, failed, needs-auth, pending, disabled — and the shim's own cadence discovers the change",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionMcpServer.health=connected/failed/needs_auth/pending/disabled",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "mcp-all" }, "fake mcp catalog (all healths) turn");
    ctx.setMcpArm("all");
    conclude(ctx, "Every MCP health is now reported.");
  },
});

export const MCP_HEALTHY = scenario({
  name: "mcp-healthy",
  prompt: "!mcp-healthy",
  emits: "prose only. It narrows `mcpServerStatus()` to the single connected server, so the arms CHANGE rather than merely existing",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionMcpServer.health=connected only — the change is what a push tests",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "mcp-healthy" }, "fake mcp catalog (healthy only) turn");
    ctx.setMcpArm("healthy");
    conclude(ctx, "Only the healthy MCP server is now reported.");
  },
});

/** One account-usage scenario per declared outcome. */
function usageScenario(name: string, arm: AccountUsageArm, armDoc: string) {
  return scenario({
    name,
    prompt: `!${name}`,
    emits: `prose only; it switches the account-usage answer to the \`${arm}\` shape`,
    writes: "the assistant line, the prompt line and the turn record",
    arms: armDoc,
    run(ctx) {
      ctx.log({ turn: ctx.turn, branch: name, usage_arm: arm }, "fake account-usage arm switched");
      ctx.setAccountUsageArm(arm);
      conclude(ctx, `The account-usage probe now answers with the ${arm} shape.`);
    },
  });
}

export const USAGE_AVAILABLE = usageScenario(
  "usage-available",
  "available",
  "SessionAccountUsage.outcome=available with five_hour, seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped and extra_usage",
);
/**
 * The SAME available shape under the name the e2e roster reaches for.
 *
 * Two names for one arm is deliberate: `!usage-available` names the ARM and
 * `!usage-full` names what a reader wants from it — every window populated, each
 * with a utilization and a reset instant, plus `subscription_type` and the
 * session cost rollup. Renaming the older one would have broken every caller
 * that already spells it.
 */
export const USAGE_FULL = usageScenario(
  "usage-full",
  "available",
  "SessionAccountUsage.outcome=available with EVERY window populated — five_hour, seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped and extra_usage, each with utilization and resets_at — beside subscription_type",
);
export const USAGE_OPUS_ABSENT = usageScenario(
  "usage-opus-absent",
  "opus_absent",
  "SessionAccountUsage.outcome=available with seven_day_opus UNSET — an absent optional window, which is not an unavailability",
);
export const USAGE_SERVICE_UNAVAILABLE = usageScenario(
  "usage-service-unavailable",
  "service_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=service_unavailable",
);
export const USAGE_WINDOW_UNAVAILABLE = usageScenario(
  "usage-window-unavailable",
  "window_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=window_unavailable — the FIVE-HOUR window is null, which is what that reason means",
);
export const USAGE_UTILIZATION_UNAVAILABLE = usageScenario(
  "usage-utilization-unavailable",
  "utilization_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=utilization_unavailable",
);
export const USAGE_SAMPLING_FAILURE = usageScenario(
  "usage-sampling-failure",
  "sampling_failure",
  "SessionAccountUsage.outcome=unavailable reason=sampling_failure",
);

export const RATE_LIMIT = scenario({
  name: "rate-limit",
  prompt: "!rate-limit",
  emits: "a `rate_limit_event` in the corpus's shape — allowed_warning on the overage window with a threshold",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionAccountUsage from the rate-limit event, plus the usage-warning synthesized notice",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "rate-limit" }, "fake rate-limit-event turn");
    ctx.emit({
      type: "rate_limit_event",
      rate_limit_info: {
        status: "allowed_warning",
        resetsAt: Math.floor(ctx.nowMs() / 1000) + 3_600,
        rateLimitType: "overage",
        utilization: 0.79,
        isUsingOverage: false,
        overageInUse: true,
        surpassedThreshold: 0.75,
      },
    });
    conclude(ctx, "The account is approaching its overage threshold.");
  },
});

/**
 * One rate-limit event per NAMED WINDOW.
 *
 * The footer draws the account's standing window, and which window a status is
 * about is the whole join: a five-hour limit and a seven-day limit are two
 * different rows on that surface, and a mock that only ever named the overage
 * window could not exercise either. `SessionRateLimitType` is a oneof, so the
 * arm IS the window and there is nothing else to read it from.
 */
function rateLimitWindowScenario(
  name: string,
  vendorWindow: string,
  arm: string,
  utilization: number,
  resetsInSeconds: number,
) {
  return scenario({
    name,
    prompt: `!${name}`,
    emits:
      `a \`rate_limit_event\` naming the \`${vendorWindow}\` window — \`allowed_warning\` with a utilization and a ` +
      "reset instant, the shape the FOOTER's window row joins against",
    writes: "the assistant line, the prompt line and the turn record",
    arms: `SessionRateLimitStatus.rate_limit_type=${arm}`,
    run(ctx) {
      ctx.log({ turn: ctx.turn, branch: name, rate_limit_type: vendorWindow }, "fake rate-limit-window turn");
      ctx.emit({
        type: "rate_limit_event",
        rate_limit_info: {
          status: "allowed_warning",
          resetsAt: Math.floor(ctx.nowMs() / 1000) + resetsInSeconds,
          rateLimitType: vendorWindow,
          utilization,
          isUsingOverage: false,
          overageInUse: false,
          surpassedThreshold: 0.75,
        },
      });
      conclude(ctx, `The ${vendorWindow} window is ${Math.round(utilization * 100)}% used.`);
    },
  });
}

export const RATE_LIMIT_FIVE_HOUR = rateLimitWindowScenario(
  "rate-limit-five-hour",
  "five_hour",
  "five_hour",
  0.82,
  3_600,
);

export const RATE_LIMIT_SEVEN_DAY = rateLimitWindowScenario(
  "rate-limit-seven-day",
  "seven_day",
  "seven_day",
  0.61,
  259_200,
);

export const CONTEXT_TIP = scenario({
  name: "context-tip",
  prompt: "!context-tip",
  emits:
    "prose only, plus the vendor's `context_tip` ATTACHMENT — a GENERIC CLI TIP, which is what the one real " +
    "capture of this record actually is. IT IS NOT THE CONTEXT-BUDGET WARNING (ruling, landing 5): which " +
    "attachment carries that warning is on the capture run's checklist, and mapping the tip to it would draw an " +
    "unrelated tip as \"your context is filling\"",
  writes: "a `context_tip` attachment line",
  arms: "residue `attachment/context_tip` — the tip is recorded as itself, unconverted, and reaches no arm",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "context-tip" }, "fake generic context-tip turn");
    ctx.attachment({
      type: "context_tip",
      tip: {
        tip: "Set a goal for this conversation with /goal so later turns can be checked against it.",
        featureId: "goal",
        action: "/goal",
      },
    });
    conclude(ctx, "The CLI offered a tip.");
  },
});

export const COMPACT = scenario({
  name: "compact",
  prompt: "!compact",
  emits:
    "a compaction: `status{compacting}`, a `compact_boundary` carrying the full corpus `compact_metadata` " +
    "(trigger, pre/post tokens, duration, the preserved segment AND the preserved-messages uuid list), then " +
    "`status{compact_result:\"success\"}`",
  writes: "a `system:compact_boundary` line whose `logicalParentUuid` names the preserved head, plus a summary user line",
  arms: "SessionCompacting + AgentUpdate.context_cut(ContextCompacted) with trigger=requested",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "compact" }, "fake compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    const head = ctx.files.transcript.chainHead ?? ctx.newUuid();
    ctx.emit({
      type: "system",
      subtype: "compact_boundary",
      compact_metadata: {
        trigger: "manual",
        pre_tokens: 435_029,
        post_tokens: 8_639,
        duration_ms: 194_511,
        preserved_segment: { head_uuid: head, anchor_uuid: head, tail_uuid: head },
        preserved_messages: { anchor_uuid: head, uuids: [head] },
      },
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "compact_boundary",
      content: "Conversation compacted",
      isMeta: false,
      level: "info",
      logicalParentUuid: head,
      compactMetadata: {
        trigger: "manual",
        preTokens: 435_029,
        durationMs: 194_511,
        preCompactDiscoveredTools: ["Monitor", "TaskList", "TaskStop"],
        preservedSegment: { headUuid: head, anchorUuid: head, tailUuid: head },
        preservedMessages: { anchorUuid: head, uuids: [head], allUuids: [head] },
        postTokens: 8_639,
        cumulativeDroppedTokens: 705_119,
      },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.systemMessage("status", { status: null, compact_result: "success" });
    conclude(ctx, "Compacted the conversation.");
  },
});

export const COMPACT_AUTO = scenario({
  name: "compact-auto",
  prompt: "!compact-auto",
  emits: "an AUTOMATIC compaction — the same shapes with `trigger: \"auto\"`, which is the only discriminator",
  writes: "a `system:compact_boundary` line with `compactMetadata.trigger: \"auto\"`",
  arms: "AgentUpdate.context_cut(ContextCompacted) with trigger=automatic",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "compact-auto" }, "fake auto-compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    const head = ctx.files.transcript.chainHead ?? ctx.newUuid();
    ctx.emit({
      type: "system",
      subtype: "compact_boundary",
      compact_metadata: {
        trigger: "auto",
        pre_tokens: 190_000,
        post_tokens: 12_000,
        duration_ms: 42_000,
        preserved_segment: { head_uuid: head, anchor_uuid: head, tail_uuid: head },
      },
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "compact_boundary",
      content: "Conversation compacted",
      isMeta: false,
      level: "info",
      logicalParentUuid: head,
      compactMetadata: {
        trigger: "auto",
        preTokens: 190_000,
        durationMs: 42_000,
        preservedSegment: { headUuid: head, anchorUuid: head, tailUuid: head },
        postTokens: 12_000,
        cumulativeDroppedTokens: 178_000,
      },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.systemMessage("status", { status: null, compact_result: "success" });
    conclude(ctx, "The conversation was compacted automatically.");
  },
});

export const COMPACT_FAILED = scenario({
  name: "compact-failed",
  prompt: "!compact-failed",
  emits: "a compaction that FAILS: `status{compacting}` then `status{compact_result:\"failed\", compact_error}` and NO boundary",
  writes: "nothing but the prompt line and the turn record — a failed compaction cut nothing",
  arms: "AgentUpdate.context_cut(ContextCompactionFailed)",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "compact-failed" }, "fake failed-compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    ctx.systemMessage("status", {
      status: null,
      compact_result: "failed",
      compact_error: "the summarizing request was rejected",
    });
    conclude(ctx, "The compaction failed and nothing was cut.");
  },
});

export const AWAY_SUMMARY = scenario({
  name: "away-summary",
  prompt: "!away-summary",
  emits: "prose only; the vendor's recap is a `system:away_summary` transcript record",
  writes: "a `system:away_summary` line",
  arms: "vendor_specific residue — `system/away_summary`, which no conversation.v1 arm models",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "away-summary" }, "fake away-summary turn");
    // BOTH PLANES, ONE UUID (`systemRecord`): the vendor's recap is a stream
    // message AND a transcript line, and residue keyed `residue:<uuid>` is what
    // collapses the sidecar's row and the shim's into one. Appending only the
    // file left the shim blind to the record and its `system/away_summary`
    // residue never written.
    ctx.systemRecord("away_summary", {
      content: "While you were away: the offline run finished three scenarios and stopped cleanly.",
      isMeta: false,
    });
    conclude(ctx, "Recapped what happened while you were away.");
  },
});

export const RESIDUE = scenario({
  name: "residue",
  prompt: "!residue",
  emits: "prose only; it writes the two attachment records BOTH planes agree are vendor bookkeeping, not context",
  writes: "`deferred_tools_delta` and `agent_listing_delta` attachment lines",
  arms:
    "NONE — these are `StoreUnservedItem.vendor_specific{kind:\"attachment/deferred_tools_delta\"}` and " +
    "`attachment/agent_listing_delta`, dropped from every page by both planes",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "residue" }, "fake vendor-residue turn");
    ctx.attachment({
      type: "deferred_tools_delta",
      addedNames: ["WebFetch", "WebSearch"],
      addedLines: ["WebFetch", "WebSearch"],
      removedNames: [],
      readdedNames: [],
      pendingMcpServers: [],
      needsAuthMcpServers: ["needs-login"],
    });
    ctx.attachment({
      type: "agent_listing_delta",
      addedTypes: ["general-purpose", "Explore"],
      addedLines: ["- general-purpose: the offline general agent", "- Explore: the offline read-only search agent"],
      removedTypes: [],
      isInitial: true,
      showConcurrencyNote: true,
    });
    conclude(ctx, "Wrote the two bookkeeping attachments.");
  },
});

export const COLD_SEED = scenario({
  name: "cold-seed",
  prompt: "!cold-seed",
  emits:
    "an ordinary turn whose TRANSCRIPT RECORDS are stamped TWO HOURS IN THE PAST, so the next resume of this " +
    "session trips the shim's own cold-context detection",
  writes: "the ASSISTANT line (with its usage) and the turn_duration line, both carrying a two-hour-old `timestamp`",
  arms: "SessionColdLapsed on the NEXT resume — this scenario only seeds the condition",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "cold-seed" }, "fake cold-context seeding turn");
    const twoHoursAgo = new Date(ctx.nowMs() - 2 * 60 * 60 * 1_000).toISOString();
    // THE LINE THE COLD GATE ACTUALLY READS is the last ASSISTANT line: its
    // `message.usage` is the context size and its `timestamp` is the request
    // instant, read from the same record. Back-dating only the turn_duration
    // line left a freshly-stamped assistant line as the newest one, so the gate
    // saw a session seconds old and never lapsed.
    ctx.assistant([{ type: "text", text: "An answer from two hours ago." }], {
      stopReason: "end_turn",
      timestamp: twoHoursAgo,
    });
    // Back-dated too, so nothing in the file contradicts it. Written directly
    // rather than through a helper because the scenario is deliberately lying
    // about WHEN, and only about when — every other field is the ordinary one.
    ctx.files.transcript.append({
      type: "system",
      subtype: "turn_duration",
      durationMs: 1_000,
      messageCount: 1,
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: twoHoursAgo,
    });
    ctx.result({ subtype: "success", result: "An answer from two hours ago." });
  },
});

export const SESSION_SCENARIOS = [
  ROTATE,
  SLASH_LOCAL,
  CONTEXT_USAGE_DRIFT,
  MODEL_FALLBACK,
  FAST_ON,
  FAST_OFF,
  FAST_COOLDOWN,
  MCP_ALL,
  MCP_HEALTHY,
  USAGE_AVAILABLE,
  USAGE_FULL,
  USAGE_OPUS_ABSENT,
  USAGE_SERVICE_UNAVAILABLE,
  USAGE_WINDOW_UNAVAILABLE,
  USAGE_UTILIZATION_UNAVAILABLE,
  USAGE_SAMPLING_FAILURE,
  RATE_LIMIT,
  RATE_LIMIT_FIVE_HOUR,
  RATE_LIMIT_SEVEN_DAY,
  CONTEXT_TIP,
  COMPACT,
  COMPACT_AUTO,
  COMPACT_FAILED,
  AWAY_SUMMARY,
  RESIDUE,
  COLD_SEED,
];
