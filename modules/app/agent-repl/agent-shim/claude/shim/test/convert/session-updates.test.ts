/**
 * FACTS ABOUT THE SESSION, NOT ABOUT ANY AGENT.
 *
 * The load-bearing rule in this file is that a value the vendor added after this
 * contract was written leaves an arm UNSET rather than being sorted into the
 * nearest one: defaulting a rate-limit status to `allowed` would tell a user
 * they have room they may not have, and attributing a status to the wrong window
 * would say their weekly allowance is nearly spent when it was the five-hour.
 */
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import {
  clearedCutEntry,
  compactionEntry,
  convertSessionMessage,
  fastModeUpdate,
  type PendingClear,
  type PendingCompaction,
} from "../../src/convert/session-updates.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { foldContext } from "./fold-harness.js";

/** One vendor record, read as the fold reads it. */
function message(fields: Record<string, unknown>): SdkMessage {
  return { uuid: "uuid-1", session_id: "session-1", ...fields } as unknown as SdkMessage;
}

/** What one session message converts to. */
function convert(
  fields: Record<string, unknown>,
  sink?: (pending: PendingCompaction) => void,
): readonly PersistEntry[] {
  return convertSessionMessage(message(fields), foldContext(), sink);
}

/** The session update a row carries. */
function updateOf(entry: PersistEntry | undefined): conversationv1.SessionUpdate | undefined {
  return entry?.item.kind === "session_update" ? entry.item.update : undefined;
}

/** The rate-limit status one `rate_limit_event` states. */
function rateLimit(info: Record<string, unknown> | undefined): conversationv1.SessionRateLimitStatus {
  const entries = convert({ type: "rate_limit_event", rate_limit_info: info });
  return updateOf(entries[0])?.update.value as conversationv1.SessionRateLimitStatus;
}

// ---------------------------------------------------------------------------
// The rate-limit event
// ---------------------------------------------------------------------------

describe("the three-value status vocabulary, which IS in evidence", () => {
  const cases = [
    { status: "allowed", arm: "allowed" },
    { status: "allowed_warning", arm: "allowedWarning" },
    { status: "rejected", arm: "rejected" },
  ] as const;

  for (const { status, arm } of cases) {
    it(`draws \`${status}\` as the ${arm} arm`, () => {
      expect(rateLimit({ status }).status.case).toBe(arm);
    });
  }

  it("leaves the arm UNSET for a value the vendor added later", () => {
    // Defaulting to `allowed` would tell a user they have room they may not have.
    expect(rateLimit({ status: "throttled_soon" }).status.case).toBeUndefined();
  });

  it("leaves the arm unset when the event stated no status at all", () => {
    expect(rateLimit({}).status.case).toBeUndefined();
  });

  it("leaves the arm unset when the event carried no info object at all", () => {
    expect(rateLimit(undefined).status.case).toBeUndefined();
  });
});

describe("which window a status is about", () => {
  const cases = [
    { value: "five_hour", arm: "fiveHour" },
    { value: "seven_day", arm: "sevenDay" },
    { value: "seven_day_opus", arm: "sevenDayOpus" },
    { value: "seven_day_sonnet", arm: "sevenDaySonnet" },
    { value: "seven_day_overage_included", arm: "sevenDayOverageIncluded" },
    { value: "overage", arm: "overage" },
  ] as const;

  for (const { value, arm } of cases) {
    it(`draws \`${value}\` as the ${arm} window`, () => {
      expect(rateLimit({ rateLimitType: value }).rateLimitType?.window.case).toBe(arm);
    });
  }

  it("states NO window for a value this contract does not spell, rather than guessing", () => {
    expect(rateLimit({ rateLimitType: "thirty_day" }).rateLimitType).toBeUndefined();
  });

  it("states no window when the vendor named none", () => {
    expect(rateLimit({}).rateLimitType).toBeUndefined();
  });
});

describe("the conversions the proto names", () => {
  it("carries the vendor's SECONDS as the unix millis the wire carries", () => {
    expect(rateLimit({ resetsAt: 1_700_000_000 }).resetsAtMs).toBe(1_700_000_000_000n);
  });

  it("leaves the reset instant unset when the vendor stated a non-number", () => {
    expect(rateLimit({ resetsAt: "soon" }).resetsAtMs).toBeUndefined();
  });

  it("carries the vendor's FRACTION as the percent the wire carries", () => {
    expect(rateLimit({ utilization: 0.42 }).utilizationPercent).toBeCloseTo(42);
  });

  it("leaves utilization unset when the vendor stated a non-finite number", () => {
    expect(rateLimit({ utilization: Number.NaN }).utilizationPercent).toBeUndefined();
  });
});

describe("the flags and words the vendor may or may not state", () => {
  it("carries a stated boolean", () => {
    expect(rateLimit({ isUsingOverage: true }).isUsingOverage).toBe(true);
  });

  it("leaves a boolean unset when the vendor stated a non-boolean", () => {
    expect(rateLimit({ isUsingOverage: "yes" }).isUsingOverage).toBeUndefined();
  });

  it("carries a stated error code", () => {
    expect(rateLimit({ errorCode: "over_quota" }).errorCode).toBe("over_quota");
  });

  it("leaves an error code unset when the vendor stated the empty string", () => {
    expect(rateLimit({ errorCode: "" }).errorCode).toBeUndefined();
  });
});

describe("the overage side of the account", () => {
  it("is absent entirely when the vendor reported none of its three facts", () => {
    expect(rateLimit({ status: "allowed" }).overage).toBeUndefined();
  });

  it("appears with an UNSET status when only the reset instant was reported", () => {
    const overage = rateLimit({ overageResetsAt: 1_000 }).overage;

    expect(overage?.status.case).toBeUndefined();
  });

  it("carries the overage status the vendor named", () => {
    expect(rateLimit({ overageStatus: "rejected" }).overage?.status.case).toBe("rejected");
  });

  it("carries the vendor's disabled reason VERBATIM, since none of the thirteen are observed", () => {
    expect(rateLimit({ overageDisabledReason: "no_payment_method" }).overage?.disabledReason).toBe(
      "no_payment_method",
    );
  });
});

describe("a rate-limit event with no uuid of its own", () => {
  it("REFUSES to key a row on an empty identity rather than colliding with every other", () => {
    expect(() =>
      convertSessionMessage({ type: "rate_limit_event" } as unknown as SdkMessage, foldContext()),
    ).toThrow(/the vendor record uuid is empty/);
  });
});

// ---------------------------------------------------------------------------
// Fast mode
// ---------------------------------------------------------------------------

describe("fastModeUpdate", () => {
  it("draws `on` as the on arm", () => {
    const fast = fastModeUpdate("on", undefined).update.value as conversationv1.SessionFastMode;
    expect(fast.state.case).toBe("on");
  });

  it("draws `cooldown` as the cooldown arm", () => {
    const fast = fastModeUpdate("cooldown", undefined).update.value as conversationv1.SessionFastMode;
    expect(fast.state.case).toBe("cooldown");
  });

  it("carries the vendor's reason on the off arm", () => {
    const fast = fastModeUpdate("off", "rate limited").update.value as conversationv1.SessionFastMode;
    const off = fast.state.value as conversationv1.SessionFastModeOff;
    expect(off.reason).toBe("rate limited");
  });

  it("states an empty reason when the vendor gave none", () => {
    const fast = fastModeUpdate("off", undefined).update.value as conversationv1.SessionFastMode;
    const off = fast.state.value as conversationv1.SessionFastModeOff;
    expect(off.reason).toBe("");
  });
});

// ---------------------------------------------------------------------------
// The session's identity rotating
// ---------------------------------------------------------------------------

describe("conversation_reset", () => {
  it("records the identity rotation with both ids", () => {
    const entries = convert({
      type: "conversation_reset",
      session_id: "old",
      new_conversation_id: "new",
    });

    const rotated = updateOf(entries[0])?.update.value as conversationv1.SessionIdentityRotated;
    expect([rotated.previousVendorSessionId, rotated.vendorSessionId]).toEqual(["old", "new"]);
  });

  it("states an empty next id when the vendor named no new conversation", () => {
    const entries = convert({ type: "conversation_reset", session_id: "old" });

    const rotated = updateOf(entries[0])?.update.value as conversationv1.SessionIdentityRotated;
    expect(rotated.vendorSessionId).toBe("");
  });

  it("states an empty previous id when the vendor named no session it rotated away from", () => {
    const entries = convert({
      type: "conversation_reset",
      session_id: undefined,
      new_conversation_id: "new",
    });

    const rotated = updateOf(entries[0])?.update.value as conversationv1.SessionIdentityRotated;
    expect(rotated.previousVendorSessionId).toBe("");
  });

  it("writes no cut on the reset itself, which does not name the cut", () => {
    // The reset's uuids are this plane's alone: the FILE plane's only evidence
    // of a clear is the `/clear` envelope, in a transcript the reset never
    // names. The cut waits for the init that states the session it rotated to.
    const entries = convert({ type: "conversation_reset", new_conversation_id: "new" });

    expect(entries).toHaveLength(1);
  });

  it("hands the held clear to the sink, so the fold can release it", () => {
    const held: PendingClear[] = [];

    convertSessionMessage(
      message({ type: "conversation_reset", new_conversation_id: "new" }),
      foldContext(),
      undefined,
      (pending) => held.push(pending),
    );

    expect(held).toEqual([{ vendorUuid: "uuid-1" }]);
  });

  it("cuts the conversation so the reader sees WHERE it was cleared", () => {
    const entry = clearedCutEntry(foldContext(), { vendorUuid: "uuid-1" }, "session-2");

    expect(entry.source.discriminator).toBe("agent_update.context_cut.cleared");
  });

  it("keys the clear's cut on the session it rotated to, as the sidecar spells it", () => {
    // The file plane reads the `/clear` envelope out of `<session-2>.jsonl` and
    // mints `session:context_cut:session-2`; a different spelling here would
    // leave one clear in the book twice.
    const entry = clearedCutEntry(foldContext(), { vendorUuid: "uuid-1" }, "session-2");

    expect(entry.upsertKey).toBe("session:context_cut:session-2");
  });

  it("names the reset record it converted as the cut's provenance", () => {
    // IDENTITY AND PROVENANCE ARE DIFFERENT FACTS: the key says which cut, the
    // source says which record this plane read to state it.
    const entry = clearedCutEntry(foldContext(), { vendorUuid: "uuid-1" }, "session-2");

    expect(entry.source.vendorUuid).toBe("uuid-1");
  });
});

// ---------------------------------------------------------------------------
// init
// ---------------------------------------------------------------------------

describe("the session opening", () => {
  it("records one row per MCP server the session knows", () => {
    const entries = convert({
      type: "system",
      subtype: "init",
      mcp_servers: [{ name: "gns", status: "connected" }],
    });

    const server = updateOf(entries[0])?.update.value as conversationv1.SessionMcpServer;
    expect(server.health.case).toBe("connected");
  });

  const healths = [
    { status: "needs-auth", arm: "needsAuth" },
    { status: "needs_auth", arm: "needsAuth" },
    { status: "pending", arm: "pending" },
    { status: "connecting", arm: "pending" },
    { status: "disabled", arm: "disabled" },
  ] as const;

  for (const { status, arm } of healths) {
    it(`draws an MCP server's \`${status}\` as the ${arm} arm`, () => {
      const entries = convert({
        type: "system",
        subtype: "init",
        mcp_servers: [{ name: "gns", status }],
      });

      const server = updateOf(entries[0])?.update.value as conversationv1.SessionMcpServer;
      expect(server.health.case).toBe(arm);
    });
  }

  it("carries an unrecognized status as the FAILED arm's error, never as healthy", () => {
    const entries = convert({
      type: "system",
      subtype: "init",
      mcp_servers: [{ name: "gns", status: "exploded" }],
    });

    const server = updateOf(entries[0])?.update.value as conversationv1.SessionMcpServer;
    const failed = server.health.value as conversationv1.SessionMcpServerFailed;
    expect(failed.error).toBe("exploded");
  });

  it("records nothing when the vendor sent no MCP server list at all", () => {
    expect(convert({ type: "system", subtype: "init" })).toEqual([]);
  });

  it("drops a server entry that names no name", () => {
    expect(
      convert({ type: "system", subtype: "init", mcp_servers: [{ status: "connected" }] }),
    ).toEqual([]);
  });

  it("records the fast-mode fact the session opened with", () => {
    const entries = convert({ type: "system", subtype: "init", fast_mode_state: "on" });

    expect(entries[0]?.source.discriminator).toBe("session_update.fast_mode");
  });

  it("carries the fast-mode disabled reason the session stated", () => {
    const entries = convert({
      type: "system",
      subtype: "init",
      fast_mode_state: "off",
      fast_mode_disabled_reason: "account setting",
    });

    const fast = updateOf(entries[0])?.update.value as conversationv1.SessionFastMode;
    const off = fast.state.value as conversationv1.SessionFastModeOff;
    expect(off.reason).toBe("account setting");
  });

  it("states no reason when the vendor spelled it as something other than a string", () => {
    const entries = convert({
      type: "system",
      subtype: "init",
      fast_mode_state: "off",
      fast_mode_disabled_reason: 7,
    });

    const fast = updateOf(entries[0])?.update.value as conversationv1.SessionFastMode;
    const off = fast.state.value as conversationv1.SessionFastModeOff;
    expect(off.reason).toBe("");
  });
});

// ---------------------------------------------------------------------------
// status and the compaction boundary
// ---------------------------------------------------------------------------

describe("the vendor's status signal", () => {
  it("records that a compaction BEGAN, so a surface can draw the in-progress state", () => {
    const entries = convert({ type: "system", subtype: "status", status: "compacting" });

    expect(entries[0]?.source.discriminator).toBe("session_update.compacting");
  });

  it("consumes a status carrying no conversation fact", () => {
    expect(convert({ type: "system", subtype: "status", status: "idle" })).toEqual([]);
  });
});

describe("the compaction boundary", () => {
  it("holds the boundary rather than emitting a cut with no summary", () => {
    const held: PendingCompaction[] = [];
    const entries = convert(
      {
        type: "system",
        subtype: "compact_boundary",
        compact_metadata: { pre_tokens: 100, post_tokens: 20, duration_ms: 5, trigger: "auto" },
      },
      (pending) => held.push(pending),
    );

    expect(entries).toEqual([]);
    expect(held[0]?.tokensBefore).toBe(100n);
  });

  it("marks an `auto` boundary automatic", () => {
    const held: PendingCompaction[] = [];
    convert(
      { type: "system", subtype: "compact_boundary", compact_metadata: { trigger: "auto" } },
      (pending) => held.push(pending),
    );

    expect(held[0]?.automatic).toBe(true);
  });

  it("marks a boundary the user asked for as NOT automatic", () => {
    const held: PendingCompaction[] = [];
    convert(
      { type: "system", subtype: "compact_boundary", compact_metadata: { trigger: "manual" } },
      (pending) => held.push(pending),
    );

    expect(held[0]?.automatic).toBe(false);
  });

  it("states zero tokens when the vendor's metadata carried no figures", () => {
    const held: PendingCompaction[] = [];
    convert({ type: "system", subtype: "compact_boundary" }, (pending) => held.push(pending));

    expect([held[0]?.tokensBefore, held[0]?.tokensAfter, held[0]?.durationMs]).toEqual([0n, 0n, 0n]);
  });

  it("produces nothing when there is nowhere to hold the boundary until its summary lands", () => {
    expect(convert({ type: "system", subtype: "compact_boundary" })).toEqual([]);
  });

  it("names the summary record by the preserved messages' anchor", () => {
    // Arrange: both captures give the summary record this anchor's uuid.
    const held: PendingCompaction[] = [];

    // Act.
    convert(
      {
        type: "system",
        subtype: "compact_boundary",
        compact_metadata: {
          trigger: "manual",
          preserved_segment: { anchor_uuid: "uuid-segment-anchor" },
          preserved_messages: { anchor_uuid: "uuid-summary", uuids: [] },
        },
      },
      (pending) => held.push(pending),
    );

    // Assert: `preserved_messages` supersedes `preserved_segment`.
    expect(held[0]?.summaryUuid).toBe("uuid-summary");
  });

  it("falls back to the preserved segment's anchor when no preserved messages are stated", () => {
    // Arrange.
    const held: PendingCompaction[] = [];

    // Act.
    convert(
      {
        type: "system",
        subtype: "compact_boundary",
        compact_metadata: { trigger: "manual", preserved_segment: { anchor_uuid: "uuid-segment-anchor" } },
      },
      (pending) => held.push(pending),
    );

    // Assert.
    expect(held[0]?.summaryUuid).toBe("uuid-segment-anchor");
  });

  it("names no summary record when the anchor is the boundary itself", () => {
    // Arrange: a prefix-preserving partial compaction anchors on the boundary.
    const held: PendingCompaction[] = [];

    // Act.
    convert(
      {
        type: "system",
        subtype: "compact_boundary",
        compact_metadata: { trigger: "manual", preserved_messages: { anchor_uuid: "uuid-1", uuids: [] } },
      },
      (pending) => held.push(pending),
    );

    // Assert.
    expect(held[0]?.summaryUuid).toBeUndefined();
  });

  it("names no summary record when the boundary states no anchor", () => {
    // Arrange.
    const held: PendingCompaction[] = [];

    // Act.
    convert({ type: "system", subtype: "compact_boundary", compact_metadata: { trigger: "auto" } }, (pending) =>
      held.push(pending),
    );

    // Assert.
    expect(held[0]?.summaryUuid).toBeUndefined();
  });

  it("stamps when the boundary was held, so a late release can state its age", () => {
    // Arrange.
    const held: PendingCompaction[] = [];

    // Act.
    convertSessionMessage(
      message({ type: "system", subtype: "compact_boundary" }),
      foldContext({ nowMs: 4_242 }),
      (pending) => held.push(pending),
    );

    // Assert.
    expect(held[0]?.heldAtMs).toBe(4_242);
  });
});

describe("compactionEntry", () => {
  /** One held boundary. */
  function pending(automatic: boolean): PendingCompaction {
    return {
      vendorUuid: "uuid-boundary",
      tokensBefore: 100n,
      tokensAfter: 20n,
      automatic,
      durationMs: 7n,
      heldAtMs: 1_000,
    };
  }

  /** The cut a compaction row carries. */
  function cutOf(entry: PersistEntry): conversationv1.ContextCompacted {
    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const cut = update.value as conversationv1.ContextCut;
    return cut.cut.value as conversationv1.ContextCompacted;
  }

  it("shows the summary in the cut's place, so the cut is not a hole", () => {
    expect(cutOf(compactionEntry(foldContext(), pending(true), "we did things")).summary?.markdown).toBe(
      "we did things",
    );
  });

  it("leaves the summary absent when the vendor stated none, rather than filling it", () => {
    expect(cutOf(compactionEntry(foldContext(), pending(true), undefined)).summary).toBeUndefined();
  });

  it("draws an automatic compaction as the automatic trigger", () => {
    expect(cutOf(compactionEntry(foldContext(), pending(true), "x")).trigger.case).toBe("automatic");
  });

  it("draws a compaction the user asked for as the requested trigger", () => {
    expect(cutOf(compactionEntry(foldContext(), pending(false), "x")).trigger.case).toBe("requested");
  });

  it("keys the row by the vendor record that stated the boundary, spelled as the sidecar spells it", () => {
    // `cut:<uuid>` here and `session:context_cut:<uuid>` on the file plane left
    // one compaction as TWO store rows, and the feed drew two dividers.
    expect(compactionEntry(foldContext(), pending(true), "x").upsertKey).toBe(
      "session:context_cut:uuid-boundary",
    );
  });
});

// ---------------------------------------------------------------------------
// The records that carry no conversation fact
// ---------------------------------------------------------------------------

describe("records relayed by other frames", () => {
  it("consumes the vendor's idle/running state, which the turn's own terminal states", () => {
    expect(convert({ type: "system", subtype: "session_state_changed", state: "idle" })).toEqual([]);
  });

  it("records nothing for an API retry, which neither ended nor failed the turn", () => {
    expect(convert({ type: "system", subtype: "api_retry", attempt: 2 })).toEqual([]);
  });
});

describe("records this contract deliberately does not carry", () => {
  it("keeps a local slash command's output as vendor-specific residue", () => {
    const entries = convert({ type: "system", subtype: "local_command_output" });

    expect(entries[0]?.source.discriminator).toBe(
      "residue.vendor_specific.local_command_output",
    );
  });

  it("keeps a subtype no converter owns as vendor-specific residue named for it", () => {
    const entries = convert({ type: "system", subtype: "teleport_begin" });

    expect(entries[0]?.source.discriminator).toBe(
      "residue.vendor_specific.system/teleport_begin",
    );
  });
});
