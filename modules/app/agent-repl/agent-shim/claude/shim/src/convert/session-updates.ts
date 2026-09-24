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
import { contextCutUpsertKey, sessionUpsertKey } from "../store/keys.js";
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
    turn: context.turnId,
    item: { kind: "session_update", update },
  };
}

// ---------------------------------------------------------------------------
// The pending clear — a cut whose IDENTITY the reset record does not carry
// ---------------------------------------------------------------------------

/**
 * A `/clear` whose ROTATED-TO session id has not been announced yet.
 *
 * WHY THE ROW CANNOT BE WRITTEN AT THE RESET. A cut is ONE fact and the two
 * planes both state it, so they must mint the SAME upsert key or the store
 * holds two rows for one cut and the feed draws the divider twice. For a
 * compaction that is easy — the vendor writes the identical `compact_boundary`,
 * uuid and all, to both planes. FOR A CLEAR IT IS NOT: the vendor hands the
 * planes DISJOINT records. `identity-rotation-clear` (2026-09-01) has the
 * stream's `conversation_reset` at uuid `cc07c2a0-…` and the file plane's only
 * evidence of the clear — the expanded `/clear` command envelope — at
 * `04f97c00-…`, in a transcript the reset does not name. Neither uuid is
 * derivable from the other, and `new_conversation_id` is a third uuid NOTHING
 * ever uses.
 *
 * The one thing both planes DO hold is THE SESSION THE CLEAR ROTATED TO: the
 * sidecar reads the envelope out of `<new session>.jsonl` and keys on that
 * file's session uuid, and this plane learns the very same id from the SECOND
 * `system:init`, which follows the reset by milliseconds and is the only place
 * the vendor states it. So the reset is HELD — bounded to one value, cleared on
 * use, exactly like the compaction below — and the row is written when that
 * init lands.
 */
export interface PendingClear {
  /** The `conversation_reset` record this cut came from — its provenance. */
  readonly vendorUuid: string;
}

/**
 * The clear's cut row, once the init that names the rotated-to session lands.
 *
 * PROVENANCE AND IDENTITY ARE DIFFERENT FACTS HERE: `source.vendorUuid` names
 * the reset record this plane converted, and the upsert key names the CUT,
 * which is the session it rotated to — the one spelling the file plane can also
 * reach.
 */
export function clearedCutEntry(
  context: FoldContext,
  pending: PendingClear,
  vendorSessionId: string,
): PersistEntry {
  LOGGER.info(
    { uuid: pending.vendorUuid, vendor_session_id: vendorSessionId },
    "the context was cleared; recording the cut keyed on the session it rotated to",
  );
  return pageLineEntry(
    context,
    {
      agentId: context.mainAgentId,
      vendorUuid: pending.vendorUuid,
      discriminator: "agent_update.context_cut.cleared",
    },
    contextCutUpsertKey(vendorSessionId),
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
  );
}

// ---------------------------------------------------------------------------
// The pending compaction — the one bounded join this file makes
// ---------------------------------------------------------------------------

/**
 * A compaction whose SUMMARY has not arrived yet.
 *
 * WHERE THE SUMMARY COMES FROM. The vendor states the boundary and then, as the
 * very next stream record, the summary itself: a MAIN-stream `user` record
 * marked `isSynthetic`, whose uuid is the one the transcript gives its
 * `isCompactSummary` line and the one `compact_metadata`'s `anchor_uuid` names
 * (`testdata/captures/compaction-directed` and `auto-compaction`, both). It is
 * NOT the assistant prose that follows: a `/compact` turn has none at all —
 * `compaction-directed` goes boundary, summary, the command's stdout replay,
 * `result` — so a cut keyed to prose was held into the NEXT turn and paired
 * with whatever the model said there.
 *
 * So exactly one boundary is remembered until that record lands, bounded by the
 * vendor's own sequence, and the turn's terminal releases it regardless (the
 * fold owns that backstop). Bounded to one value, cleared on use.
 */
export interface PendingCompaction {
  readonly vendorUuid: string;
  readonly tokensBefore: bigint;
  readonly tokensAfter: bigint;
  readonly automatic: boolean;
  readonly durationMs: bigint;
  /**
   * The uuid the summary record carries, when the boundary names it: the
   * `anchor_uuid` of its preserved segment. UNSET when the boundary names no
   * anchor (the vendor summarized everything) or names ITSELF as the anchor (a
   * prefix-preserving partial compaction), and then the summary is the first
   * main-stream synthetic user record that follows.
   */
  readonly summaryUuid?: string;
  /** When the boundary was held, for the age a late release reports. */
  readonly heldAtMs: number;
}

/**
 * The compaction row.
 *
 * `summary` is UNSET when the vendor never stated one for this boundary — the
 * fold releases such a cut at the turn's terminal, or when a second boundary
 * supersedes it, with the facts the boundary itself carried. Nothing stands in
 * for the missing summary: the field is left absent rather than filled.
 */
export function compactionEntry(
  context: FoldContext,
  pending: PendingCompaction,
  summary: string | undefined,
): PersistEntry {
  LOGGER.info(
    {
      uuid: pending.vendorUuid,
      tokens_before: pending.tokensBefore.toString(),
      tokens_after: pending.tokensAfter.toString(),
      summarized: summary !== undefined,
    },
    summary === undefined
      ? "the conversation was compacted; recording the cut without a summary, since the vendor stated none"
      : "the conversation was compacted; recording the cut with its summary",
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
              ...(summary === undefined ? {} : { summary: prose(summary) }),
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
      // warn: a defect because an unknown rate-limit status leaves the wire arm unset.
      LOGGER.warn(
        { status: String(value) },
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
      // warn: a defect because an unknown rate-limit window leaves the wire field unset.
      LOGGER.warn(
        // JSON, not String: the vendor sends `unknown` here, and the one shape
        // worth logging — an object this contract did not expect — is exactly
        // the shape `String` flattens to "[object Object]".
        { rate_limit_type: JSON.stringify(value) ?? typeof value },
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
 * caller (the fold) holds the one pending value and emits the row when the
 * vendor's summary record lands, or at the turn's terminal at the latest.
 *
 * `clearSink` receives a reset whose ROTATED-TO session id has not been stated
 * yet, on the same terms: the fold holds it and emits the row when the init
 * that names that id lands. See `PendingClear` for why the reset itself cannot
 * name the cut.
 */
export function convertSessionMessage(
  message: SdkMessage,
  context: FoldContext,
  compactionSink?: (pending: PendingCompaction) => void,
  clearSink?: (pending: PendingClear) => void,
): readonly PersistEntry[] {
  const record = message as unknown as Record<string, unknown>;
  const uuid = typeof record.uuid === "string" ? record.uuid : "";
  const subtype = typeof record.subtype === "string" ? record.subtype : undefined;

  if (message.type === "rate_limit_event") {
    LOGGER.debug({ uuid }, "the vendor stated the account's live rate-limit status");
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
    LOGGER.info({ previous, next }, "the vendor rotated the session's identity and cleared the context");
    const rotated = create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "identityRotated",
        value: create(conversationv1.SessionIdentityRotatedSchema, {
          previousVendorSessionId: previous,
          vendorSessionId: next,
        }),
      },
    });
    // THE CUT IS HELD, NOT WRITTEN HERE. Its upsert key is the session the
    // clear rotated to, which this record does not name; `PendingClear` says
    // why, and the init that follows releases it.
    if (clearSink === undefined) {
      // warn: a defect because a reset without a fold cannot retain its pending session cut.
      LOGGER.warn(
        { uuid },
        "a conversation reset arrived with nowhere to hold it until the init names the session it rotated to",
      );
    } else {
      clearSink({ vendorUuid: uuid });
    }
    return [sessionEntry(context, uuid, "identity_rotated", rotated)];
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
      LOGGER.info(
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
        LOGGER.info({ uuid }, "the vendor began compacting the context");
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
      const summaryUuid = summaryAnchor(metadata, uuid);
      const pending: PendingCompaction = {
        vendorUuid: uuid,
        tokensBefore: typeof before === "number" ? BigInt(Math.trunc(before)) : 0n,
        tokensAfter: typeof after === "number" ? BigInt(Math.trunc(after)) : 0n,
        automatic: metadata?.trigger === "auto",
        durationMs: typeof duration === "number" ? BigInt(Math.trunc(duration)) : 0n,
        ...(summaryUuid === undefined ? {} : { summaryUuid }),
        heldAtMs: context.nowMs(),
      };
      if (compactionSink === undefined) {
        // warn: a defect because a compaction without a fold cannot retain its pending summary cut.
        LOGGER.warn(
          { uuid },
          "a compaction boundary arrived with nowhere to hold it until its summary lands",
        );
        return [];
      }
      LOGGER.debug(
        { uuid, summary_uuid: summaryUuid },
        "a compaction boundary arrived; holding it until its summary record lands",
      );
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
      LOGGER.debug(
        {
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
      LOGGER.debug({ uuid }, "the vendor answered a local slash command; recorded as vendor-specific residue");
      return [
        residueEntry(
          context,
          message,
          vendorSpecificResidue(residueKind({ type: "system", subtype }), message),
          "residue.vendor_specific.local_command_output",
        ),
      ];

    default:
      LOGGER.debug(
        { uuid, type: message.type, subtype },
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

/**
 * The uuid a boundary's summary record will carry, when the boundary names it.
 *
 * `preserved_messages` supersedes `preserved_segment` (sdk.d.ts), so its anchor
 * is read first. An anchor equal to the boundary's own uuid is a
 * prefix-preserving partial compaction, where the anchor is the boundary and
 * not the summary, so it names nothing here.
 */
function summaryAnchor(metadata: Record<string, unknown> | undefined, boundaryUuid: string): string | undefined {
  const anchorOf = (segment: unknown): string | undefined => {
    const anchor = (segment as { anchor_uuid?: unknown } | undefined)?.anchor_uuid;
    return typeof anchor === "string" && anchor !== "" ? anchor : undefined;
  };
  const anchor = anchorOf(metadata?.preserved_messages) ?? anchorOf(metadata?.preserved_segment);
  return anchor === boundaryUuid ? undefined : anchor;
}
