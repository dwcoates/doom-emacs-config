/**
 * convert/session-updates.ts — facts about the SESSION, not about any agent.
 *
 * # What belongs here and what does not
 *
 * A session update is a fact the whole session carries: its identity rotated,
 * an MCP server's health changed, the account's allowance was observed. Nothing
 * about a turn or a unit rides here — each of those has a stream of its own.
 *
 * SHIM-SYNTHESIZED FACTS ARE NEVER WRITTEN TO THE STORE. `diagnostics` and
 * `context_usage` are the shim's report ABOUT ITSELF at its own cadence; they
 * are pushed on WatchSession and they are not vendor conversation, so they never
 * reach a row. They are also not produced here: the engine pushes them.
 *
 * # The context cut is a PAGE LINE, not a session update
 *
 * A `/clear` and a compaction cut the CONVERSATION, and the reader must see
 * where — so they land as `AgentUpdate.context_cut` in the main agent's book,
 * drawn as the separation divider. The vendor's `status: compacting` signal is
 * the one session-scoped half of it: it says a compaction BEGAN, so a surface
 * can draw the in-progress state, and the ContextCut record is the end.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { sessionUpsertKey } from "../store/keys.js";
import { pageLineEntry, prose } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import { residueEntry, residueKind, vendorSpecificResidue } from "./residue.js";

const LOGGER = bindLog({ component: "shim-convert-session", operation: "shim.convert.session" });

/** One session fact as a row. */
function sessionEntry(
  context: FoldContext,
  vendorUuid: string,
  arm: string,
  update: conversationv1.SessionUpdate,
): PersistEntry {
  return {
    agentId: context.mainAgentId,
    upsertKey: sessionUpsertKey(arm, vendorUuid),
    source: { vendorUuid, discriminator: `session_update.${arm}` },
    keepalive: context.keepalive,
    item: { kind: "session_update", update },
  };
}

/** The upsert key a context cut takes: the vendor record that stated it. */
function contextCutUpsertKey(vendorUuid: string): string {
  return `cut:${vendorUuid}`;
}

// ---------------------------------------------------------------------------
// The pending compaction — the one bounded join this file makes
// ---------------------------------------------------------------------------

/**
 * A compaction whose SUMMARY has not arrived yet.
 *
 * `ContextCompacted.summary` is not optional — the feed shows the summary in
 * the cut's place so the cut is not a hole — and the vendor states the boundary
 * FIRST and the summary as the assistant message that follows. So exactly one
 * boundary is remembered until that message lands, and the row is emitted then.
 * Bounded to one value, cleared on use.
 */
export interface PendingCompaction {
  readonly vendorUuid: string;
  readonly tokensBefore: bigint;
  readonly tokensAfter: bigint;
  readonly automatic: boolean;
  readonly durationMs: bigint;
}

/** The compaction row, once its summary has arrived. */
export function compactionEntry(
  context: FoldContext,
  pending: PendingCompaction,
  summary: string,
): PersistEntry {
  LOGGER.log(
    { tokens_before: pending.tokensBefore.toString(), tokens_after: pending.tokensAfter.toString() },
    "the conversation was compacted; recording the cut with its summary",
  );
  return pageLineEntry(
    context,
    {
      agentId: context.mainAgentId,
      vendorUuid: pending.vendorUuid,
      discriminator: "agent_update.context_cut.compacted",
    },
    contextCutUpsertKey(pending.vendorUuid),
    create(conversationv1.AgentUpdateSchema, {
      update: {
        case: "contextCut",
        value: create(conversationv1.ContextCutSchema, {
          cut: {
            case: "compacted",
            value: create(conversationv1.ContextCompactedSchema, {
              summary: prose(summary),
              tokens: create(conversationv1.ContextTokenDeltaSchema, {
                tokensBefore: pending.tokensBefore,
                tokensAfter: pending.tokensAfter,
              }),
              trigger: pending.automatic
                ? {
                    case: "automatic",
                    value: create(conversationv1.ContextCompactionAutomaticSchema, {}),
                  }
                : {
                    case: "requested",
                    value: create(conversationv1.ContextCompactionRequestedSchema, {}),
                  },
              durationMs: pending.durationMs,
            }),
          },
        }),
      },
    }),
  );
}

// ---------------------------------------------------------------------------
// Per-message converters
// ---------------------------------------------------------------------------

/** One MCP server's health, as the session states it. */
function mcpServerUpdate(name: string, status: string): conversationv1.SessionUpdate {
  const health: conversationv1.SessionMcpServer["health"] =
    status === "connected"
      ? { case: "connected", value: create(conversationv1.SessionMcpServerConnectedSchema, {}) }
      : status === "needs-auth" || status === "needs_auth"
        ? { case: "needsAuth", value: create(conversationv1.SessionMcpServerNeedsAuthSchema, {}) }
        : status === "pending" || status === "connecting"
          ? { case: "pending", value: create(conversationv1.SessionMcpServerPendingSchema, {}) }
          : status === "disabled"
            ? { case: "disabled", value: create(conversationv1.SessionMcpServerDisabledSchema, {}) }
            : {
                case: "failed",
                value: create(conversationv1.SessionMcpServerFailedSchema, { error: status }),
              };
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "mcpServer",
      value: create(conversationv1.SessionMcpServerSchema, { name, health }),
    },
  });
}

/** The vendor's fast-mode state, as the session states it. */
export function fastModeUpdate(state: string, reason: string | undefined): conversationv1.SessionUpdate {
  const arm: conversationv1.SessionFastMode["state"] =
    state === "on"
      ? { case: "on", value: create(conversationv1.SessionFastModeOnSchema, {}) }
      : state === "cooldown"
        ? { case: "cooldown", value: create(conversationv1.SessionFastModeCooldownSchema, {}) }
        : {
            case: "off",
            value: create(conversationv1.SessionFastModeOffSchema, { reason: reason ?? "" }),
          };
  return create(conversationv1.SessionUpdateSchema, {
    update: { case: "fastMode", value: create(conversationv1.SessionFastModeSchema, { state: arm }) },
  });
}

// ---------------------------------------------------------------------------
// The vendor's rate-limit event
// ---------------------------------------------------------------------------

/**
 * The three-value status vocabulary, which IS in evidence.
 *
 * A value the vendor adds later leaves the arm UNSET rather than becoming one of
 * these: an unset status says "the vendor said something we do not model", and
 * defaulting to `allowed` would tell a user they have room they may not have.
 */
function rateLimitStatus(
  value: unknown,
): conversationv1.SessionRateLimitStatus["status"] {
  switch (value) {
    case "allowed":
      return { case: "allowed", value: create(conversationv1.SessionRateLimitAllowedSchema, {}) };
    case "allowed_warning":
      return {
        case: "allowedWarning",
        value: create(conversationv1.SessionRateLimitAllowedWarningSchema, {}),
      };
    case "rejected":
      return { case: "rejected", value: create(conversationv1.SessionRateLimitRejectedSchema, {}) };
    default:
      LOGGER.log(
        { level: "warn", status: String(value) },
        "the vendor named a rate-limit status this contract does not spell; the arm stays unset",
      );
      return { case: undefined };
  }
}

/** Which window a status is about; the vendor's declared six-value vocabulary. */
function rateLimitType(value: unknown): conversationv1.SessionRateLimitType | undefined {
  const window: conversationv1.SessionRateLimitType["window"] | undefined =
    value === "five_hour"
      ? { case: "fiveHour", value: create(conversationv1.SessionRateLimitWindowFiveHourSchema, {}) }
      : value === "seven_day"
        ? { case: "sevenDay", value: create(conversationv1.SessionRateLimitWindowSevenDaySchema, {}) }
        : value === "seven_day_opus"
          ? {
              case: "sevenDayOpus",
              value: create(conversationv1.SessionRateLimitWindowSevenDayOpusSchema, {}),
            }
          : value === "seven_day_sonnet"
            ? {
                case: "sevenDaySonnet",
                value: create(conversationv1.SessionRateLimitWindowSevenDaySonnetSchema, {}),
              }
            : value === "seven_day_overage_included"
              ? {
                  case: "sevenDayOverageIncluded",
                  value: create(
                    conversationv1.SessionRateLimitWindowSevenDayOverageIncludedSchema,
                    {},
                  ),
                }
              : value === "overage"
                ? {
                    case: "overage",
                    value: create(conversationv1.SessionRateLimitWindowOverageSchema, {}),
                  }
                : undefined;
  if (window === undefined) {
    // UNSET, never guessed: a status attributed to the wrong window would tell
    // a user their weekly allowance is nearly spent when it was the five-hour.
    if (value !== undefined) {
      LOGGER.log(
        { level: "warn", rate_limit_type: String(value) },
        "the vendor named a rate-limit window this contract does not spell; the field stays unset",
      );
    }
    return undefined;
  }
  return create(conversationv1.SessionRateLimitTypeSchema, { window });
}

/** The vendor's SECONDS as the unix millis the wire carries. */
function resetsAtMs(seconds: unknown): bigint | undefined {
  if (typeof seconds !== "number" || !Number.isFinite(seconds)) return undefined;
  return BigInt(Math.trunc(seconds * 1_000));
}

/** The vendor's FRACTION (0–1) as the percent (0–100) the wire carries. */
function percentOf(fraction: unknown): number | undefined {
  if (typeof fraction !== "number" || !Number.isFinite(fraction)) return undefined;
  return fraction * 100;
}

/** A boolean the vendor stated, or UNSET when it stated none. */
function flag(value: unknown): boolean | undefined {
  return typeof value === "boolean" ? value : undefined;
}

/** A string the vendor stated, or UNSET when it stated none. */
function text(value: unknown): string | undefined {
  return typeof value === "string" && value !== "" ? value : undefined;
}

/** The overage side of the account, when the vendor reported it. */
function rateLimitOverage(
  info: Record<string, unknown>,
): conversationv1.SessionRateLimitOverage | undefined {
  const status = info.overageStatus;
  const resets = info.overageResetsAt;
  const reason = info.overageDisabledReason;
  if (status === undefined && resets === undefined && reason === undefined) return undefined;
  return create(conversationv1.SessionRateLimitOverageSchema, {
    status: status === undefined ? { case: undefined } : rateLimitStatus(status),
    resetsAtMs: resetsAtMs(resets),
    // THIRTEEN DECLARED VALUES AND NONE OBSERVED, so the vendor's word is
    // carried verbatim rather than sorted into arms nobody has seen produced.
    disabledReason: text(reason),
  });
}

/**
 * The vendor's rate-limit event, typed.
 *
 * STREAM-ONLY (landing 4): this is the live `rate_limit_event` the SDK pushes,
 * and it is a different fact from `account_usage`, which is the sampled answer
 * to the vendor's usage verb. Two facts, two arms — folding one into the other
 * would make a live status and a sample indistinguishable.
 *
 * EVERY OPTIONAL FIELD IS ABSENT WHEN THE VENDOR OMITTED IT. The conversions are
 * the two the proto names: seconds→millis, fraction→percent.
 */
function rateLimitStatusUpdate(
  info: Record<string, unknown> | undefined,
): conversationv1.SessionUpdate {
  const record = info ?? {};
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "rateLimitStatus",
      value: create(conversationv1.SessionRateLimitStatusSchema, {
        status: rateLimitStatus(record.status),
        resetsAtMs: resetsAtMs(record.resetsAt),
        rateLimitType: rateLimitType(record.rateLimitType),
        utilizationPercent: percentOf(record.utilization),
        overage: rateLimitOverage(record),
        isUsingOverage: flag(record.isUsingOverage),
        overageInUse: flag(record.overageInUse),
        surpassedThresholdPercent: percentOf(record.surpassedThreshold),
        errorCode: text(record.errorCode),
        canUserPurchaseCredits: flag(record.canUserPurchaseCredits),
        hasChargeableSavedPaymentMethod: flag(record.hasChargeableSavedPaymentMethod),
      }),
    },
  });
}

/**
 * Every session-scoped message the vendor sends.
 *
 * `compactionSink` receives a boundary whose summary has not arrived yet; the
 * caller (the fold) holds the one pending value and emits the row when the next
 * assistant message supplies the summary.
 */
export function convertSessionMessage(
  message: SdkMessage,
  context: FoldContext,
  compactionSink?: (pending: PendingCompaction) => void,
): readonly PersistEntry[] {
  const record = message as unknown as Record<string, unknown>;
  const uuid = typeof record.uuid === "string" ? record.uuid : "";
  const subtype = typeof record.subtype === "string" ? record.subtype : undefined;

  if (message.type === "rate_limit_event") {
    LOGGER.log({ uuid }, "the vendor stated the account's live rate-limit status");
    return [
      sessionEntry(
        context,
        uuid,
        "rate_limit_status",
        rateLimitStatusUpdate(record.rate_limit_info as Record<string, unknown> | undefined),
      ),
    ];
  }

  if (message.type === "conversation_reset") {
    const next = typeof record.new_conversation_id === "string" ? record.new_conversation_id : "";
    const previous = typeof record.session_id === "string" ? record.session_id : "";
    LOGGER.log({ previous, next }, "the vendor rotated the session's identity and cleared the context");
    const rotated = create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "identityRotated",
        value: create(conversationv1.SessionIdentityRotatedSchema, {
          previousVendorSessionId: previous,
          vendorSessionId: next,
        }),
      },
    });
    return [
      sessionEntry(context, uuid, "identity_rotated", rotated),
      pageLineEntry(
        context,
        {
          agentId: context.mainAgentId,
          vendorUuid: uuid,
          discriminator: "agent_update.context_cut.cleared",
        },
        contextCutUpsertKey(uuid),
        create(conversationv1.AgentUpdateSchema, {
          update: {
            case: "contextCut",
            value: create(conversationv1.ContextCutSchema, {
              cut: {
                case: "cleared",
                // The vendor's reset record carries NO token delta, so
                // `ContextCleared` states nothing — which is the honest shape,
                // and why its only field was retired.
                value: create(conversationv1.ContextClearedSchema, {}),
              },
            }),
          },
        }),
      ),
    ];
  }

  switch (subtype) {
    case "init": {
      const servers = Array.isArray(record.mcp_servers)
        ? (record.mcp_servers as { name?: unknown; status?: unknown }[])
        : [];
      const entries: PersistEntry[] = servers
        .filter((server) => typeof server.name === "string" && typeof server.status === "string")
        .map((server) =>
          sessionEntry(
            context,
            `${uuid}:${String(server.name)}`,
            "mcp_server",
            mcpServerUpdate(String(server.name), String(server.status)),
          ),
        );
      const fastMode = record.fast_mode_state;
      if (typeof fastMode === "string") {
        entries.push(
          sessionEntry(
            context,
            uuid,
            "fast_mode",
            fastModeUpdate(
              fastMode,
              typeof record.fast_mode_disabled_reason === "string"
                ? record.fast_mode_disabled_reason
                : undefined,
            ),
          ),
        );
      }
      LOGGER.log(
        { uuid, mcp_servers: servers.length, fast_mode: fastMode },
        "the session opened; recording its MCP and fast-mode facts",
      );
      // The model and the permission mode are SessionStarted's, not a session
      // update's: they are facts AT START, and stating them twice would give the
      // consumer two authorities on one question.
      return entries;
    }

    case "status": {
      const status = record.status;
      // THE FAILED CUT IS THE ENGINE'S. It holds the compaction it asked for,
      // so it produces `context_cut.compaction_failed` from this same
      // `compact_result: "failed"` record and the fold must not double-produce
      // it. What stays here is the SUCCESS cut, which needs the boundary's real
      // figures and the summary that follows — facts only the fold sees.
      if (status === "compacting") {
        LOGGER.log({ uuid }, "the vendor began compacting the context");
        return [
          sessionEntry(
            context,
            uuid,
            "compacting",
            create(conversationv1.SessionUpdateSchema, {
              update: {
                case: "compacting",
                value: create(conversationv1.SessionCompactingSchema, {}),
              },
            }),
          ),
        ];
      }
      LOGGER.logVerbose({ uuid, status }, "a status message with no conversation fact; consumed");
      return [];
    }

    case "compact_boundary": {
      const metadata = record.compact_metadata as Record<string, unknown> | undefined;
      const before = metadata?.pre_tokens;
      const after = metadata?.post_tokens;
      const duration = metadata?.duration_ms;
      const pending: PendingCompaction = {
        vendorUuid: uuid,
        tokensBefore: typeof before === "number" ? BigInt(Math.trunc(before)) : 0n,
        tokensAfter: typeof after === "number" ? BigInt(Math.trunc(after)) : 0n,
        automatic: metadata?.trigger === "auto",
        durationMs: typeof duration === "number" ? BigInt(Math.trunc(duration)) : 0n,
      };
      if (compactionSink === undefined) {
        LOGGER.log(
          { level: "warn", uuid },
          "a compaction boundary arrived with nowhere to hold it until its summary lands",
        );
        return [];
      }
      LOGGER.log({ uuid }, "a compaction boundary arrived; holding it until its summary lands");
      compactionSink(pending);
      return [];
    }

    case "session_state_changed":
      // The vendor's idle/running/requires_action state has NO arm in
      // SessionUpdate: the turn's own terminal is what says a turn ended, and a
      // second authority on that question is exactly what the contract avoids.
      LOGGER.logVerbose({ uuid, state: record.state }, "session state relayed by the turn's own frames; consumed");
      return [];

    case "api_retry":
      // NOTHING ON THE WIRE, BY DESIGN: a retried request did not end the turn
      // and did not fail it, so there is no conversation fact yet. The evidence
      // that matters lands as the turn's own terminal if the retries run out.
      LOGGER.log(
        {
          level: "warn",
          uuid,
          attempt: record.attempt,
          max_retries: record.max_retries,
          error_status: record.error_status,
        },
        "the vendor is retrying an API request; logged and not recorded",
      );
      return [];

    case "local_command_output":
      // THE VENDOR ANSWERED A SLASH COMMAND ITSELF. The daemon recognizes every
      // session command before it reaches the shim, so this is output for a
      // command this system did not send, and it has no unit identity to upsert
      // under. Understood and deliberately not carried: vendor-specific residue.
      LOGGER.log({ uuid }, "the vendor answered a local slash command; recorded as vendor-specific residue");
      return [
        residueEntry(
          context,
          message,
          vendorSpecificResidue(residueKind({ type: "system", subtype }), message),
          "residue.vendor_specific.local_command_output",
        ),
      ];

    default:
      LOGGER.log(
        { level: "warn", uuid, type: message.type, subtype },
        "no session converter owns this vendor record; recorded as vendor-specific residue",
      );
      return [
        residueEntry(
          context,
          message,
          vendorSpecificResidue(residueKind(record), message),
          `residue.vendor_specific.${residueKind(record)}`,
        ),
      ];
  }
}
