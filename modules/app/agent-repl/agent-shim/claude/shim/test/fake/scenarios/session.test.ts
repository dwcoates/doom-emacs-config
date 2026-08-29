/**
 * The session-fact family: rotation, slash commands the vendor answers itself,
 * fast mode, compaction, and the two attachment kinds both planes drop.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, recordsOfType, theResult } from "../harness.js";

describe("identity rotation", () => {
  it("announces the new conversation id on the stream", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const reset = ofType(driven, "conversation_reset")[0];

    // Assert
    expect(typeof reset?.new_conversation_id).toBe("string");
  });

  it("follows the reset with a fresh init reporting the NEW id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const resetIndex = (driven.messages as unknown as Record<string, unknown>[]).findIndex(
      (m) => m.type === "conversation_reset",
    );
    const init = (driven.messages as unknown as Record<string, unknown>[])[resetIndex + 1];
    const newId = (driven.messages as unknown as Record<string, unknown>[])[resetIndex]?.new_conversation_id;

    // Assert
    expect({ type: init?.type, subtype: init?.subtype, id: init?.session_id }).toEqual({
      type: "system",
      subtype: "init",
      id: newId,
    });
  });

  it("carries the turn's own result under the NEW identity", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const newId = String(ofType(driven, "conversation_reset")[0]?.new_conversation_id);

    // Assert. The turn STARTED under one id and ENDS under another; that split
    // is the whole shape rotation handling exists for.
    expect(theResult(driven).session_id).toBe(newId);
  });

  it("leaves the retired transcript intact with a closing record", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const oldLines = driven.transcript("sess-fake-1");

    // Assert
    expect(oldLines.at(-1)).toMatchObject({ type: "system", subtype: "compact_boundary" });
  });

  it("starts a new transcript file whose chain begins fresh", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const newId = String(ofType(driven, "conversation_reset")[0]?.new_conversation_id);

    // Assert
    expect(driven.transcript(newId)[0]).toMatchObject({ parentUuid: null, sessionId: newId });
  });
});

describe("a vendor-answered slash command", () => {
  it("streams the output as local_command_output", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash"]);

    // Assert
    expect(ofType(driven, "system", "local_command_output")).toHaveLength(1);
  });

  it("records it wrapped in local-command-stdout", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "local_command",
    );

    // Assert
    expect(String(record?.content)).toMatch(/^<local-command-stdout>[\s\S]*<\/local-command-stdout>$/);
  });
});

describe("fast mode", () => {
  it("reports each state on the turn's result", async () => {
    // Arrange + Act
    const states: unknown[] = [];
    for (const prompt of ["!fast-on", "!fast-off", "!fast-cooldown"]) {
      states.push(theResult(await driveScenario([prompt])).fast_mode_state);
    }

    // Assert
    expect(states).toEqual(["on", "off", "cooldown"]);
  });

  it("names the disabled reason when fast mode is not on", async () => {
    // Arrange + Act
    const off = theResult(await driveScenario(["!fast-off"]));

    // Assert
    expect(off.fast_mode_disabled_reason).toBe("preference");
  });

  it("omits the disabled reason when fast mode IS on", async () => {
    // Arrange + Act
    const on = theResult(await driveScenario(["!fast-on"]));

    // Assert
    expect(on.fast_mode_disabled_reason).toBeUndefined();
  });
});

describe("rate-limit events", () => {
  it("emits the corpus's overage-warning shape", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit"]);
    const event = ofType(driven, "rate_limit_event")[0]?.rate_limit_info as Record<string, unknown>;

    // Assert
    expect({ status: event.status, type: event.rateLimitType, threshold: event.surpassedThreshold }).toEqual({
      status: "allowed_warning",
      type: "overage",
      threshold: 0.75,
    });
  });
});

describe("the context-budget warning", () => {
  it("is an attachment, never a stream message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!context-budget"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
    };

    // Assert
    expect(attachment.type).toBe("context_tip");
  });
});

describe("compaction", () => {
  it("brackets the boundary with a compacting status and a success status", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact"]);
    const statuses = ofType(driven, "system", "status");

    // Assert
    expect(statuses.map((s) => s.status ?? s.compact_result)).toEqual(["compacting", "success"]);
  });

  it("carries the preserved segment AND the preserved-message uuid list", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact"]);
    const boundary = ofType(driven, "system", "compact_boundary")[0]?.compact_metadata as Record<
      string,
      unknown
    >;

    // Assert. The uuid list SUPERSEDES the segment for readers; both are sent.
    expect({
      segment: boundary.preserved_segment !== undefined,
      messages: boundary.preserved_messages !== undefined,
    }).toEqual({ segment: true, messages: true });
  });

  it("distinguishes an automatic compaction only by its trigger", async () => {
    // Arrange + Act
    const manual = await driveScenario(["!compact"]);
    const auto = await driveScenario(["!compact-auto"]);
    const triggerOf = (d: typeof manual): unknown =>
      (ofType(d, "system", "compact_boundary")[0]?.compact_metadata as { trigger: string }).trigger;

    // Assert
    expect([triggerOf(manual), triggerOf(auto)]).toEqual(["manual", "auto"]);
  });

  it("records the boundary with logicalParentUuid naming the preserved head", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "compact_boundary",
    );

    // Assert
    expect(typeof record?.logicalParentUuid).toBe("string");
  });

  it("emits NO boundary when the compaction failed", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact-failed"]);

    // Assert. A failed compaction cut nothing, so there is nothing to bound.
    expect(ofType(driven, "system", "compact_boundary")).toHaveLength(0);
  });

  it("names the failure on the closing status", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact-failed"]);
    const closing = ofType(driven, "system", "status").at(-1);

    // Assert
    expect({ result: closing?.compact_result, hasError: typeof closing?.compact_error === "string" }).toEqual(
      { result: "failed", hasError: true },
    );
  });
});

describe("vendor bookkeeping attachments", () => {
  it("writes the two records both planes drop from every page", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!residue"]);
    const types = recordsOfType(driven.transcript(), "attachment").map(
      (l) => (l.attachment as { type: string }).type,
    );

    // Assert. These are vendor_specific residue, never context_injected: they
    // say what the model MAY call, not what it read.
    expect(types).toEqual(["deferred_tools_delta", "agent_listing_delta"]);
  });
});

describe("the away summary", () => {
  it("is a system record with no stream message beside it", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!away-summary"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "away_summary",
    );

    // Assert
    expect({ recorded: record !== undefined, streamed: ofType(driven, "system", "away_summary").length }).toEqual(
      { recorded: true, streamed: 0 },
    );
  });
});

describe("cold-context seeding", () => {
  it("stamps the turn's closing record TWO HOURS in the past", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cold-seed"]);
    const stamps = recordsOfType(driven.transcript(), "system")
      .filter((l) => l.subtype === "turn_duration")
      .map((l) => Date.parse(String(l.timestamp)));
    const now = 1_800_000_000_000;

    // Assert. The oldest stamp is what the shim's cold gate reads on the next
    // resume of this session.
    expect(Math.min(...stamps)).toBe(now - 2 * 60 * 60 * 1_000);
  });

  it("still ends the turn successfully, so the seeding is invisible to the turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cold-seed"]);

    // Assert
    expect(theResult(driven).subtype).toBe("success");
  });

  it("leaves a transcript a resume can continue the chain from", async () => {
    // Arrange + Act
    const seeded = await driveScenario(["!cold-seed"]);
    const lastUuid = seeded.transcript().at(-1)?.uuid;

    // Assert
    expect(typeof lastUuid).toBe("string");
  });
});
