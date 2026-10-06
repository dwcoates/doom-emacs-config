/**
 * fake/scenarios/session.ts — the facts a session states about ITSELF.
 *
 * # Most SessionUpdate arms are ANSWERS, not messages
 *
 * `sdk.d.ts` declares no `model_changed`, no `permission_mode_changed`, no
 * `fast_mode`, no `mcp_server` and no `account_usage` system message. Those
 * arms are produced by the shim
 * from CONTROL ANSWERS (`mcpServerStatus`, the account-usage probe,
 * `getContextUsage`) and from fields that ride other messages
 * (`status.permissionMode`, `result.fast_mode_state`, `init.fast_mode_state`).
 *
 * So the scenarios here do two different things. Where a message exists, they
 * emit it. Where only an answer exists, they FLIP THE ANSWER the mock will give
 * (`setAccountUsageArm`, `setMcpArm`) and let the shim's own cadence discover
 * it — which is the real production path, not a shortcut around it.
 */
import type { AccountUsageArm, ScenarioContext } from "../scenario.js";
import { conclude, scenario, withheldThinking } from "./support.js";

const ROTATE = scenario({
  name: "rotate",
  prompt: "!rotate",
  emits:
    "a `/clear` in the OBSERVED shape: ONE `conversation_reset` carrying the OLD `session_id` and a " +
    "`new_conversation_id` nothing later uses, then a SECOND `system:init` whose `session_id` is the REAL new id " +
    "(a third uuid), and the REST of the turn — its result included — belongs to that identity",
  writes:
    "a NEW `<new-session>.jsonl` opening with the harness's local-command trio — the `<local-command-caveat>` " +
    "isMeta record, the `<command-name>/clear</command-name>` ENVELOPE (the clear's only file-plane record), and " +
    "an empty `system:local_command` — and carrying everything after the reset; the OLD file simply STOPS, with " +
    "no closing record of any kind",
  arms: "SessionIdentityRotated + AgentUpdate.context_cut(ContextCleared)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "rotate" }, "fake identity-rotation turn");
    const next = ctx.rotate();
    ctx.log.debug({ new_claude_session_id: next }, "fake vendor minted a new conversation identity");
    conclude(ctx, "Cleared the conversation.");
  },
});

const SLASH_LOCAL = scenario({
  name: "slash",
  prompt: "!slash",
  emits:
    "a slash command the VENDOR answers itself: a `local_command_output` message, and the transcript's " +
    "`system:local_command` record wrapping the output in `<local-command-stdout>`",
  writes: "a `system:local_command` line and a `command_permissions` attachment line",
  arms: "the vendor-answered slash-command family — no agent activity beyond the answer, and NO reasoning",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "slash" }, "fake vendor-answered slash-command turn");
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
    // NO REASONING PRELUDE, unlike every other turn: the vendor ANSWERED this
    // one itself, and the `vendor-answered-slash-commands` capture folds into a
    // response and nothing else. Going through `conclude` would put a thinking
    // unit in front of an answer the model never composed.
    const conclusion = "Answered the slash command locally.";
    ctx.assistant([{ type: "text", text: conclusion }], { stopReason: "end_turn" });
    ctx.result({ subtype: "success", result: conclusion });
  },
});

/**
 * "Shape A": the CLI's own slash-command bookkeeping, recorded as a raw
 * `user`-typed transcript record (never a content-block array — the corpus's
 * own local-command records carry a plain string).
 */
function slashShapeAContent(name: string): string {
  return `<command-message>${name}</command-message>\n<command-name>/${name}</command-name>\n<command-args></command-args>`;
}

const SLASH_SHAPE_A = scenario({
  name: "slash-shape-a",
  prompt: "!slash-shape-a [command]",
  emits:
    "NOTHING on the stream — this is a FILE-PLANE-ONLY shape. It writes a \"user\"-typed `TranscriptLine` whose " +
    "content is the CLI's own slash-command bookkeeping (`<command-message>{name}</command-message>\\n` " +
    "`<command-name>/{name}</command-name>\\n<command-args></command-args>`), parameterized by command name",
  writes: "one `user` transcript line carrying a fresh `promptId`, then the ordinary prompt line and the turn record",
  arms: "none in this converter (nothing reaches the stream); the SIDECAR classifies the record on the file plane — a command the CLI answers itself is `user/slash_command` residue, never a prompt (shim-sidecar internal/convert/bookkeeping.go)",
  run(ctx) {
    const name = ctx.args === "" ? "compact" : ctx.args;
    ctx.log.debug({ turn: ctx.turn, branch: "slash-shape-a", command: name }, "fake Shape-A slash-command bookkeeping turn");
    // A KNOB, not a fixed value: `ctx.newUuid()` is deterministic in the test
    // harness, so a caller can name exactly which `promptId` this record will
    // carry without the scenario hard-coding one.
    ctx.files.transcript.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: { role: "user", content: slashShapeAContent(name) },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    conclude(ctx, `Recorded the Shape-A bookkeeping for /${name}.`);
  },
});

const SLASH_SHAPE_A_UNNAMED = scenario({
  name: "slash-shape-a-unnamed",
  prompt: "!slash-shape-a-unnamed",
  emits:
    "NOTHING on the stream — the same FILE-PLANE-ONLY \"user\"-typed record, but the WITHHELD-UNNAMED shape: only " +
    "`<local-command-stdout>...</local-command-stdout>`, with no `<command-name>` element at all",
  writes: "one `user` transcript line carrying a fresh `promptId`, then the ordinary prompt line and the turn record",
  arms: "none in this converter (nothing reaches the stream); the SIDECAR classifies the record on the file plane as `user/local_command_output` residue, never a prompt (shim-sidecar internal/convert/bookkeeping.go)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "slash-shape-a-unnamed" }, "fake Shape-A withheld-unnamed bookkeeping turn");
    ctx.files.transcript.append({
      promptId: ctx.newUuid(),
      type: "user",
      message: {
        role: "user",
        content: "<local-command-stdout>total 4\ndrwxr-xr-x</local-command-stdout>",
      },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    conclude(ctx, "Recorded the withheld-unnamed bookkeeping.");
  },
});

const CONTEXT_USAGE_DRIFT = scenario({
  name: "context-usage-drift",
  prompt: "!context-usage-drift",
  emits:
    "prose only. It switches `getContextUsage()` to a GROWING answer, so the `context_usage` the shim pushes at " +
    "this turn's end differs from the one it pushed at session start — total tokens, percentage, the message " +
    "category and the whole `messageBreakdown` all move, and the answer stays a full " +
    "`SDKControlGetContextUsageResponse`. CADENCE IS THE ENGINE'S: context_usage is pushed at session start, " +
    "after every main-agent API response that carries usage, and at EVERY turn end regardless of scenario, so " +
    "this one changes what is sampled and never when",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionContextUsage — the same arm twice with DIFFERENT figures, which is what a re-render tests",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "context-usage-drift" }, "fake context-usage drift turn");
    ctx.setContextUsageDrift(true);
    conclude(ctx, "The context-usage answer now grows with every turn.");
  },
});

const MODEL_FALLBACK = scenario({
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
    ctx.log.debug({ turn: ctx.turn, branch: "model-fallback" }, "fake unsolicited model-fallback turn");
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
      ctx.log.debug({ turn: ctx.turn, branch: name, fast_mode_state: state }, "fake fast-mode turn");
      ctx.setFastMode(state, reason);
      const conclusion = `Fast mode is ${state}.`;
      // `[thinking, text]`, like every capture's closing API response — this one
      // spells it out rather than going through `conclude` because the result
      // carries the fast-mode fields.
      ctx.assistant([withheldThinking(), { type: "text", text: conclusion }], {
        stopReason: "end_turn",
      });
      ctx.result({
        subtype: "success",
        result: conclusion,
        fastModeState: state,
        ...(reason === undefined ? {} : { fastModeDisabledReason: reason }),
      });
    },
  });
}

const FAST_ON = fastModeScenario("fast-on", "on");
const FAST_OFF = fastModeScenario("fast-off", "off", "preference");
const FAST_COOLDOWN = fastModeScenario("fast-cooldown", "cooldown", "extra_usage_disabled");

const MCP_ALL = scenario({
  name: "mcp-all",
  prompt: "!mcp-all",
  emits:
    "prose only. It switches `mcpServerStatus()` to the FIVE-server catalog — one per declared health: " +
    "connected, failed, needs-auth, pending, disabled — and the shim's own cadence discovers the change",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionMcpServer.health=connected/failed/needs_auth/pending/disabled",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "mcp-all" }, "fake mcp catalog (all healths) turn");
    ctx.setMcpArm("all");
    conclude(ctx, "Every MCP health is now reported.");
  },
});

const MCP_HEALTHY = scenario({
  name: "mcp-healthy",
  prompt: "!mcp-healthy",
  emits: "prose only. It narrows `mcpServerStatus()` to the single connected server, so the arms CHANGE rather than merely existing",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionMcpServer.health=connected only — the change is what a push tests",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "mcp-healthy" }, "fake mcp catalog (healthy only) turn");
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
      ctx.log.debug({ turn: ctx.turn, branch: name, usage_arm: arm }, "fake account-usage arm switched");
      ctx.setAccountUsageArm(arm);
      conclude(ctx, `The account-usage probe now answers with the ${arm} shape.`);
    },
  });
}

const USAGE_AVAILABLE = usageScenario(
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
const USAGE_FULL = usageScenario(
  "usage-full",
  "available",
  "SessionAccountUsage.outcome=available with EVERY window populated — five_hour, seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped and extra_usage, each with utilization and resets_at — beside subscription_type",
);
const USAGE_OPUS_ABSENT = usageScenario(
  "usage-opus-absent",
  "opus_absent",
  "SessionAccountUsage.outcome=available with seven_day_opus UNSET — an absent optional window, which is not an unavailability",
);
const USAGE_SERVICE_UNAVAILABLE = usageScenario(
  "usage-service-unavailable",
  "service_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=service_unavailable",
);
const USAGE_WINDOW_UNAVAILABLE = usageScenario(
  "usage-window-unavailable",
  "window_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=window_unavailable — the FIVE-HOUR window is null, which is what that reason means",
);
const USAGE_UTILIZATION_UNAVAILABLE = usageScenario(
  "usage-utilization-unavailable",
  "utilization_unavailable",
  "SessionAccountUsage.outcome=unavailable reason=utilization_unavailable",
);
const USAGE_SAMPLING_FAILURE = usageScenario(
  "usage-sampling-failure",
  "sampling_failure",
  "SessionAccountUsage.outcome=unavailable reason=sampling_failure",
);

const USAGE_SEAT_SPEND = usageScenario(
  "usage-seat-spend",
  "seat_spend",
  "SessionAccountUsage.outcome=seat_spend with allotment and spent — an enterprise seat billed by spend, every window null",
);
const USAGE_SEAT_SPEND_UNREPORTED = usageScenario(
  "usage-seat-spend-unreported",
  "seat_spend_unreported",
  "SessionAccountUsage.outcome=seat_spend with the allotment and spent UNSET — the seat before the vendor reports any spend",
);

const RATE_LIMIT = scenario({
  name: "rate-limit",
  prompt: "!rate-limit",
  emits: "a `rate_limit_event` in the corpus's shape — allowed_warning on the overage window with a threshold",
  writes: "the assistant line, the prompt line and the turn record",
  arms: "SessionAccountUsage from the rate-limit event, plus the usage-warning synthesized notice",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "rate-limit" }, "fake rate-limit-event turn");
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
      ctx.log.debug({ turn: ctx.turn, branch: name, rate_limit_type: vendorWindow }, "fake rate-limit-window turn");
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

const RATE_LIMIT_FIVE_HOUR = rateLimitWindowScenario(
  "rate-limit-five-hour",
  "five_hour",
  "five_hour",
  0.82,
  3_600,
);

// ABOVE THE NEWSWORTHY GATE, deliberately. The footer only draws an allowance
// once its utilization reaches the newsworthy threshold (daemon's
// resolve/footer DefaultRateLimitNewsworthyThreshold, 0.8), so a weekly figure
// below it makes the seven-day allowance UNREACHABLE from this scenario — the
// window row could never be asserted. No capture in the corpus carries a
// seven-day window or a utilization figure of any kind, so there is no real
// number to prefer; the figure is chosen above the gate, and kept distinct
// from the five-hour scenario's so a join cannot pass by luck.
const RATE_LIMIT_SEVEN_DAY = rateLimitWindowScenario(
  "rate-limit-seven-day",
  "seven_day",
  "seven_day",
  0.91,
  259_200,
);

const CONTEXT_TIP = scenario({
  name: "context-tip",
  prompt: "!context-tip",
  emits:
    "prose only, plus the vendor's `context_tip` ATTACHMENT — a GENERIC CLI TIP, which is what the one real " +
    "capture of this record actually is. It is recorded as itself and never read as a context-budget warning: " +
    "no footer line warns that the context is nearly full (owner ruling, 2026-10-06)",
  writes: "a `context_tip` attachment line",
  arms: "residue `attachment/context_tip` — the tip is recorded as itself, unconverted, and reaches no arm",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "context-tip" }, "fake generic context-tip turn");
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

const TOKENS_REMINDER = scenario({
  name: "tokens-reminder",
  prompt: "!tokens-reminder",
  emits:
    "prose only, plus the vendor's `total_tokens_reminder` ATTACHMENT — the ONE token-budget carrier any real " +
    "capture holds (`artifact-publish-and-list`, once): a bare `text` field spelling " +
    "`<total_tokens>N tokens left</total_tokens>` and nothing else. It is never read as a context-budget " +
    "warning (owner ruling, 2026-10-06)",
  writes: "a `total_tokens_reminder` attachment line",
  arms:
    "NOTHING IS STORED for it: the sidecar reads and classifies the line and then drops it — " +
    "`attachment/total_tokens_reminder` is on the never-persisted list (owner ruling 2026-09-13, " +
    "shim-sidecar/internal/convert/neverpersist.go)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "tokens-reminder" }, "fake total-tokens-reminder turn");
    ctx.attachment({
      type: "total_tokens_reminder",
      // VERBATIM SHAPE from the capture: one `text` field, the count inside a
      // `<total_tokens>` element. No structured figure is offered anywhere.
      text: "<total_tokens>15000000 tokens left</total_tokens>",
    });
    conclude(ctx, "The CLI restated the token budget.");
  },
});

/**
 * The preserved-segment bookkeeping a REAL `compact_boundary` carries.
 *
 * GROUNDED, and grounded twice: `testdata/captures/compaction-directed`
 * (trigger `manual`) and `testdata/captures/auto-compaction` (trigger `auto`)
 * agree on the whole shape, so neither is a one-run accident.
 *
 *   - `head_uuid` and `tail_uuid` are the FIRST and LAST entries of
 *     `preserved_messages.uuids` — the span is a real span, several messages
 *     wide, not a point.
 *   - `anchor_uuid` is a uuid of its OWN and appears in NEITHER uuid list.
 *   - `uuids` and `all_uuids` carry the same entries.
 *
 * The fake previously collapsed all five onto the transcript's chain head, so
 * an anchor was indistinguishable from a preserved message and the head/tail
 * span did not exist at all. A consumer that reads the span, or that expects an
 * anchor outside the preserved set, could not be exercised against that shape.
 */
function preservedUuids(ctx: ScenarioContext, head: string) {
  const uuids = [head, ctx.newUuid(), ctx.newUuid()];
  const tail = uuids[uuids.length - 1];
  const anchor = ctx.newUuid();
  return {
    head,
    tail,
    anchor,
    stream: {
      preserved_segment: { head_uuid: head, anchor_uuid: anchor, tail_uuid: tail },
      preserved_messages: { anchor_uuid: anchor, uuids, all_uuids: uuids },
    },
    file: {
      preservedSegment: { headUuid: head, anchorUuid: anchor, tailUuid: tail },
      preservedMessages: { anchorUuid: anchor, uuids, allUuids: uuids },
    },
  };
}

const COMPACT = scenario({
  name: "compact",
  prompt: "!compact [summary]",
  emits:
    "a compaction: `status{compacting}`, a `compact_boundary` carrying the full corpus `compact_metadata` " +
    "(trigger, pre/post tokens, duration, the preserved segment AND the preserved-messages uuid list), then " +
    "`status{compact_result:\"success\"}`. The summary is stated as the record right after the boundary — a " +
    "synthetic main-stream `user` record whose uuid is the boundary's `anchor_uuid` — and `ContextCompacted.Summary` " +
    "is read off it (`settleCompaction`), so a caller names its own distinctive summary as the prompt's argument " +
    "instead of the fixed default",
  writes:
    "a `system:compact_boundary` line whose `logicalParentUuid` names the preserved TAIL, plus the `isCompactSummary` " +
    "user line under the anchor's uuid",
  arms: "SessionCompacting + AgentUpdate.context_cut(ContextCompacted) with trigger=requested",
  run(ctx) {
    const summary = ctx.args === "" ? "Compacted the conversation." : ctx.args;
    ctx.log.debug({ turn: ctx.turn, branch: "compact", summary }, "fake compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    const preserved = preservedUuids(ctx, ctx.files.transcript.chainHead ?? ctx.newUuid());
    const boundaryUuid = ctx.systemRecord(
      "compact_boundary",
      {
        compact_metadata: {
          trigger: "manual",
          pre_tokens: 435_029,
          post_tokens: 8_639,
          cumulative_dropped_tokens: 705_119,
          duration_ms: 194_511,
          ...preserved.stream,
        },
        // GROUNDED: in BOTH real captures `logical_parent_uuid` equals the
        // preserved TAIL, not the head — it names the message the post-boundary
        // transcript hangs off, which is the last preserved one.
        logical_parent_uuid: preserved.tail,
      },
      {
        content: "Conversation compacted",
        level: "info",
        logicalParentUuid: preserved.tail,
        compactMetadata: {
          trigger: "manual",
          preTokens: 435_029,
          durationMs: 194_511,
          ...preserved.file,
          postTokens: 8_639,
          cumulativeDroppedTokens: 705_119,
        },
      },
    );
    ctx.compactSummary(boundaryUuid, preserved.anchor, summary);
    ctx.systemMessage("status", { status: null, compact_result: "success" });
    conclude(ctx, summary);
  },
});

const COMPACT_AUTO = scenario({
  name: "compact-auto",
  prompt: "!compact-auto",
  emits: "an AUTOMATIC compaction — the same shapes with `trigger: \"auto\"`, which is the only discriminator",
  writes: "a `system:compact_boundary` line with `compactMetadata.trigger: \"auto\"`",
  arms: "AgentUpdate.context_cut(ContextCompacted) with trigger=automatic",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "compact-auto" }, "fake auto-compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    const preserved = preservedUuids(ctx, ctx.files.transcript.chainHead ?? ctx.newUuid());
    const boundaryUuid = ctx.systemRecord(
      "compact_boundary",
      {
        // GROUNDED, verbatim from `testdata/captures/auto-compaction` (Haiku,
        // 2026-09-04): sixteen paced 40000-byte reads carried the window to
        // 165716 tokens and the vendor compacted on its own. The figures were
        // invented before that capture existed.
        compact_metadata: {
          trigger: "auto",
          pre_tokens: 165_716,
          post_tokens: 13_675,
          cumulative_dropped_tokens: 152_041,
          duration_ms: 18_695,
          ...preserved.stream,
        },
        logical_parent_uuid: preserved.tail,
      },
      {
        content: "Conversation compacted",
        level: "info",
        logicalParentUuid: preserved.tail,
        compactMetadata: {
          trigger: "auto",
          preTokens: 165_716,
          durationMs: 18_695,
          ...preserved.file,
          postTokens: 13_675,
          cumulativeDroppedTokens: 152_041,
        },
      },
    );
    const autoSummary = "The conversation was compacted automatically.";
    ctx.compactSummary(boundaryUuid, preserved.anchor, autoSummary);
    ctx.systemMessage("status", { status: null, compact_result: "success" });
    conclude(ctx, autoSummary);
  },
});

const COMPACT_FAILED = scenario({
  name: "compact-failed",
  prompt: "!compact-failed",
  emits: "a compaction that FAILS: `status{compacting}` then `status{compact_result:\"failed\", compact_error}` and NO boundary",
  writes: "nothing but the prompt line and the turn record — a failed compaction cut nothing",
  arms: "AgentUpdate.context_cut(ContextCompactionFailed)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "compact-failed" }, "fake failed-compaction turn");
    ctx.systemMessage("status", { status: "compacting" });
    ctx.systemMessage("status", {
      status: null,
      compact_result: "failed",
      compact_error: "the summarizing request was rejected",
    });
    conclude(ctx, "The compaction failed and nothing was cut.");
  },
});

const AWAY_SUMMARY = scenario({
  name: "away-summary",
  prompt: "!away-summary",
  emits: "prose only; the vendor's recap is a `system:away_summary` transcript record",
  writes: "a `system:away_summary` line",
  arms: "vendor_specific residue — `system/away_summary`, which no conversation.v1 arm models",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "away-summary" }, "fake away-summary turn");
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

const RESIDUE = scenario({
  name: "residue",
  prompt: "!residue",
  emits: "prose only; it writes the two attachment records BOTH planes agree are vendor bookkeeping, not context",
  writes: "`deferred_tools_delta` and `agent_listing_delta` attachment lines",
  arms:
    "NONE — these are `StoreUnservedItem.vendor_specific{kind:\"attachment/deferred_tools_delta\"}` and " +
    "`attachment/agent_listing_delta`, dropped from every page by both planes",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "residue" }, "fake vendor-residue turn");
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

/** The context `!cold-seed` reports: comfortably above the cold-gate floor. */
const COLD_SEED_CONTEXT_TOKENS = 90_000;

const COLD_SEED = scenario({
  name: "cold-seed",
  prompt: "!cold-seed",
  emits:
    "an ordinary turn whose assistant line reports a context ABOVE the cold-gate floor, so a resume the cold gate " +
    "judges lapsed (AGENT_REPL_FAKE_COLD_GATE_LATER_MS) or a model switch is refused rather than let through",
  writes: "the assistant line (with its usage), the prompt line and the turn record, all stamped at the fake's own now",
  arms: "SessionColdLapsed on a resume judged later than the cache window; SessionCold on a model switch",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "cold-seed" }, "fake cold-context seeding turn");
    // A CONTEXT ABOVE THE COLD-GATE FLOOR, and nothing else. The gate does not
    // ask below 70,000 tokens (owner ruling, engine/cold.ts
    // COLD_GATE_FLOOR_TOKENS), and the mock's ordinary usage reports ~25,000 —
    // a seed that left it there would produce a session that resumes warm.
    //
    // NOTHING HERE IS BACK-DATED. The lapse is the gate's reading of the time
    // passed, moved by the shim's `--fake`-only AGENT_REPL_FAKE_COLD_GATE_LATER_MS
    // (src/main.ts). Stamping the assistant line hours in the past made the
    // turn's answer precede its own prompt, which the live stream had stamped
    // now, and the book's order then depended on which producer wrote first.
    ctx.assistant([{ type: "text", text: "An answer the next resume finds expensive." }], {
      stopReason: "end_turn",
      contextTokens: COLD_SEED_CONTEXT_TOKENS,
    });
    ctx.result({ subtype: "success", result: "An answer the next resume finds expensive." });
  },
});

export const SESSION_SCENARIOS = [
  ROTATE,
  SLASH_LOCAL,
  SLASH_SHAPE_A,
  SLASH_SHAPE_A_UNNAMED,
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
  USAGE_SEAT_SPEND,
  USAGE_SEAT_SPEND_UNREPORTED,
  RATE_LIMIT,
  RATE_LIMIT_FIVE_HOUR,
  RATE_LIMIT_SEVEN_DAY,
  CONTEXT_TIP,
  TOKENS_REMINDER,
  COMPACT,
  COMPACT_AUTO,
  COMPACT_FAILED,
  AWAY_SUMMARY,
  RESIDUE,
  COLD_SEED,
];
