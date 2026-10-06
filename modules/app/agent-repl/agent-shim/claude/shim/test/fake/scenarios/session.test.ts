/**
 * The session-fact family: rotation, slash commands the vendor answers itself,
 * fast mode, compaction, and the two attachment kinds both planes drop.
 */
import { describe, expect, it } from "vitest";

import { COLD_GATE_FLOOR_TOKENS, readTranscriptFacts, transcriptPath } from "../../../src/engine/cold.js";
import { HARNESS_NOW_MS, driveScenario, ofType, recordsOfType, theResult } from "../harness.js";

describe("identity rotation", () => {
  /** Every message, as plain records, in arrival order. */
  const records = (driven: Awaited<ReturnType<typeof driveScenario>>): Record<string, unknown>[] =>
    driven.messages;

  it("announces the reset under the OLD identity", async () => {
    // OBSERVED (identity-rotation-clear, 2026-09-01): `conversation_reset`
    // carries the session id it is RETIRING, not the one it is moving to.
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const reset = ofType(driven, "conversation_reset")[0];

    // Assert
    expect(reset?.session_id).toBe("sess-fake-1");
  });

  it("announces a new_conversation_id that NOTHING later uses", async () => {
    // The third uuid. It exists on this one message and is never adopted as an
    // identity — reading it as the new session id was the old mock's mistake
    // and would have made the shim write a link file for a phantom.
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const announced = String(ofType(driven, "conversation_reset")[0]?.new_conversation_id);

    // Assert
    expect(typeof announced).toBe("string");
    expect(records(driven).filter((m) => m.session_id === announced)).toEqual([]);
  });

  it("follows the reset with a SECOND init that states the real new id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const all = records(driven);
    const resetIndex = all.findIndex((m) => m.type === "conversation_reset");
    const init = all[resetIndex + 1];
    const announced = all[resetIndex]?.new_conversation_id;

    // Assert
    expect({ type: init?.type, subtype: init?.subtype }).toEqual({
      type: "system",
      subtype: "init",
    });
    expect(init?.session_id).not.toBe("sess-fake-1");
    expect(init?.session_id).not.toBe(announced);
  });

  it("carries the turn's own result under the NEW identity", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const all = records(driven);
    const resetIndex = all.findIndex((m) => m.type === "conversation_reset");
    const newId = all[resetIndex + 1]?.session_id;

    // Assert. The turn STARTED under one id and ENDS under another; that split
    // is the whole shape rotation handling exists for.
    expect(theResult(driven).session_id).toBe(newId);
  });

  it("leaves the retired transcript with NO closing record", async () => {
    // The old file SIMPLY STOPS. The mock used to append a `compact_boundary`
    // "Conversation cleared" line, which no capture carries — and which said
    // the wrong thing besides, since compaction is IN PLACE and rotates nothing.
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const oldLines = driven.transcript("sess-fake-1");

    // Assert
    expect(oldLines.filter((line) => line.subtype === "compact_boundary")).toEqual([]);
    expect(oldLines.at(-1)?.type).not.toBe("system");
  });

  it("starts a new transcript file under the INIT's id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rotate"]);
    const all = records(driven);
    const resetIndex = all.findIndex((m) => m.type === "conversation_reset");
    const newId = String(all[resetIndex + 1]?.session_id);

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

describe("Shape-A slash-command bookkeeping", () => {
  it("writes a user-typed record with a plain-string command-message/command-name/command-args body", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash-shape-a"]);
    const record = recordsOfType(driven.transcript(), "user").find(
      (l) => typeof (l.message as { content?: unknown })?.content === "string",
    );

    // Assert
    expect((record?.message as { content: string }).content).toBe(
      "<command-message>compact</command-message>\n<command-name>/compact</command-name>\n<command-args></command-args>",
    );
  });

  it("parameterizes the command name from the prompt's argument", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash-shape-a clear"]);
    const record = recordsOfType(driven.transcript(), "user").find(
      (l) => typeof (l.message as { content?: unknown })?.content === "string",
    );

    // Assert
    expect((record?.message as { content: string }).content).toBe(
      "<command-message>clear</command-message>\n<command-name>/clear</command-name>\n<command-args></command-args>",
    );
  });

  it("carries a promptId knob distinct from the ordinary prompt line's", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash-shape-a"]);
    const lines = recordsOfType(driven.transcript(), "user");
    const shapeA = lines.find((l) => typeof (l.message as { content?: unknown })?.content === "string");
    const ordinary = lines.find((l) => Array.isArray((l.message as { content?: unknown })?.content));

    // Assert
    expect(shapeA?.promptId).toBeDefined();
    expect(shapeA?.promptId).not.toBe(ordinary?.promptId);
  });

  it("emits NOTHING on the stream — this is a file-plane-only record", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash-shape-a"]);

    // Assert. Only the ordinary assistant/result messages a `conclude` produces.
    expect(driven.messages.some((m) => (m as { type: string }).type === "attachment")).toBe(false);
  });
});

describe("the withheld-unnamed Shape-A variant", () => {
  it("writes only local-command-stdout, with no command-name element at all", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!slash-shape-a-unnamed"]);
    const record = recordsOfType(driven.transcript(), "user").find(
      (l) => typeof (l.message as { content?: unknown })?.content === "string",
    );

    // Assert
    const content = (record?.message as { content: string }).content;
    expect(content).toMatch(/^<local-command-stdout>[\s\S]*<\/local-command-stdout>$/);
    expect(content).not.toContain("<command-name>");
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

describe("the generic context tip", () => {
  it("is an attachment, never a stream message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!context-tip"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
    };

    // Assert
    expect(attachment.type).toBe("context_tip");
  });

  it("is a GENERIC tip and not the context-budget warning", async () => {
    // RULING (landing 5): the one real capture of this record is a `/goal`
    // tip, so a mock that dressed it as the budget warning would have every
    // suite agree with a mapping the vendor never made.
    const driven = await driveScenario(["!context-tip"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      tip?: { featureId?: string };
    };

    expect(attachment.tip?.featureId).toBe("goal");
  });
});

describe("the total-tokens reminder", () => {
  it("carries the ONE token-budget shape any capture holds", async () => {
    // From `artifact-publish-and-list`, the only capture with it: a bare `text`
    // field spelling the count inside a `<total_tokens>` element, and no
    // structured figure anywhere.
    // Arrange + Act
    const driven = await driveScenario(["!tokens-reminder"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as Record<
      string,
      unknown
    >;

    // Assert
    expect(attachment).toEqual({
      type: "total_tokens_reminder",
      text: "<total_tokens>15000000 tokens left</total_tokens>",
    });
  });

  it("is NOT dressed as the context-budget warning", async () => {
    // No capture carries a `context_budget_warning` record of any spelling,
    // and the warning is retired (owner ruling, 2026-10-06). Mapping the
    // nearest carrier to it would make every suite agree with a mapping the
    // vendor never made.
    // Arrange + Act
    const driven = await driveScenario(["!tokens-reminder"]);
    const types = recordsOfType(driven.transcript(), "attachment").map(
      (line) => (line.attachment as { type?: string }).type,
    );

    // Assert
    expect(types).not.toContain("context_budget_warning");
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

  /** The stream record right after the boundary, which is where the vendor states the summary. */
  function afterBoundary(driven: Awaited<ReturnType<typeof driveScenario>>): Record<string, unknown> | undefined {
    const index = driven.messages.findIndex(
      (message) => message.type === "system" && message.subtype === "compact_boundary",
    );
    return driven.messages[index + 1];
  }

  it.each(["!compact", "!compact-auto"] as const)(
    "states the summary as a synthetic main-stream user record right after the boundary (%s)",
    async (prompt) => {
      // Arrange + Act
      const driven = await driveScenario([prompt]);
      const summary = afterBoundary(driven);

      // Assert. GROUNDED: compaction-directed and auto-compaction, both.
      expect({ type: summary?.type, synthetic: summary?.isSynthetic, parent: summary?.parent_tool_use_id }).toEqual({
        type: "user",
        synthetic: true,
        parent: null,
      });
    },
  );

  it.each(["!compact", "!compact-auto"] as const)(
    "gives the summary record the uuid the boundary's anchor names (%s)",
    async (prompt) => {
      // Arrange + Act
      const driven = await driveScenario([prompt]);
      const boundary = ofType(driven, "system", "compact_boundary")[0]?.compact_metadata as {
        preserved_messages: { anchor_uuid: string };
      };

      // Assert
      expect(afterBoundary(driven)?.uuid).toBe(boundary.preserved_messages.anchor_uuid);
    },
  );

  it("writes the transcript's isCompactSummary line under the stream record's uuid, parented on the boundary", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact"]);
    const boundary = recordsOfType(driven.transcript(), "system").find((l) => l.subtype === "compact_boundary");
    const line = recordsOfType(driven.transcript(), "user").find((l) => l.isCompactSummary === true);

    // Assert
    expect({ uuid: line?.uuid, parent: line?.parentUuid }).toEqual({
      uuid: afterBoundary(driven)?.uuid,
      parent: boundary?.uuid,
    });
  });

  it("defaults the summary to the fixed text", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact"]);

    // Assert
    expect((afterBoundary(driven)?.message as { content?: unknown } | undefined)?.content).toBe(
      "Compacted the conversation.",
    );
  });

  it("lets a caller override the summary with a distinctive string", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!compact what the discarded history said"]);

    // Assert
    expect((afterBoundary(driven)?.message as { content?: unknown } | undefined)?.content).toBe(
      "what the discarded history said",
    );
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

  // GROUNDED (compaction-directed, 2026-09-03 re-capture): the real
  // `compact_boundary`/`compactMetadata` record has no
  // `preCompactDiscoveredTools` field anywhere, on either plane. That field
  // was the fake's own invention and must never reappear.
  it.each(["!compact", "!compact-auto"] as const)("never invents preCompactDiscoveredTools (%s)", async (prompt) => {
    // Arrange + Act
    const driven = await driveScenario([prompt]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "compact_boundary",
    );

    // Assert
    expect((record?.compactMetadata as Record<string, unknown> | undefined)?.preCompactDiscoveredTools).toBe(
      undefined,
    );
  });

  // GROUNDED: the real capture's compactMetadata carries `allUuids` alongside
  // `uuids`, and `cumulativeDroppedTokens` alongside `postTokens` — both on
  // the wire event's snake_case `compact_metadata` and the written
  // camelCase `compactMetadata`.
  it.each(["!compact", "!compact-auto"] as const)(
    "carries allUuids and cumulativeDroppedTokens on the written record (%s)",
    async (prompt) => {
      // Arrange + Act
      const driven = await driveScenario([prompt]);
      const record = recordsOfType(driven.transcript(), "system").find(
        (l) => l.subtype === "compact_boundary",
      );
      const metadata = record?.compactMetadata as Record<string, unknown> | undefined;

      // Assert
      expect({
        hasAllUuids: Array.isArray(metadata?.preservedMessages && (metadata.preservedMessages as Record<string, unknown>).allUuids),
        cumulativeDroppedTokens: typeof metadata?.cumulativeDroppedTokens === "number",
      }).toEqual({ hasAllUuids: true, cumulativeDroppedTokens: true });
    },
  );

  it.each(["!compact", "!compact-auto"] as const)(
    "carries all_uuids and cumulative_dropped_tokens on the wire event, plus a logical_parent_uuid (%s)",
    async (prompt) => {
      // Arrange + Act
      const driven = await driveScenario([prompt]);
      const boundary = ofType(driven, "system", "compact_boundary")[0];
      const metadata = boundary?.compact_metadata as Record<string, unknown> | undefined;

      // Assert
      expect({
        hasAllUuids: Array.isArray(
          metadata?.preserved_messages && (metadata.preserved_messages as Record<string, unknown>).all_uuids,
        ),
        cumulativeDroppedTokens: typeof metadata?.cumulative_dropped_tokens === "number",
        logicalParentUuid: typeof boundary?.logical_parent_uuid === "string",
      }).toEqual({ hasAllUuids: true, cumulativeDroppedTokens: true, logicalParentUuid: true });
    },
  );

  // GROUNDED (compaction-directed): the real run never produces a
  // `local_command_output` record anywhere — not just around the boundary.
  it.each(["!compact", "!compact-auto", "!compact-failed"] as const)(
    "never emits a local_command_output record (%s)",
    async (prompt) => {
      // Arrange + Act
      const driven = await driveScenario([prompt]);

      // Assert
      expect(ofType(driven, "system", "local_command_output")).toHaveLength(0);
      expect(recordsOfType(driven.transcript(), "local_command_output")).toHaveLength(0);
    },
  );
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
  it("lands on BOTH planes, since either producer may convert it", async () => {
    // The recap is a transcript line AND a stream message. It was file-plane
    // only, which left the shim blind to it and its `system/away_summary`
    // residue never written — while the store expects exactly that row.
    // Arrange + Act
    const driven = await driveScenario(["!away-summary"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "away_summary",
    );

    // Assert
    expect({
      recorded: record !== undefined,
      streamed: ofType(driven, "system", "away_summary").length,
    }).toEqual({ recorded: true, streamed: 1 });
  });

  it("gives the two planes ONE uuid, so their residue rows collapse into one", async () => {
    // Residue is keyed `residue:<vendor record uuid>` (landing 5) precisely so
    // the sidecar's row and the shim's row are the same row. Two uuids would
    // make the same recap appear twice.
    // Arrange + Act
    const driven = await driveScenario(["!away-summary"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "away_summary",
    );
    const streamed = ofType(driven, "system", "away_summary")[0] as { uuid?: string } | undefined;

    // Assert
    expect(streamed?.uuid).toBe(record?.uuid);
  });
});

describe("cold-context seeding", () => {
  it("stamps every transcript record at the fake's own now", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cold-seed"]);
    const stamps = driven
      .transcript()
      .filter((l) => typeof l.timestamp === "string")
      .map((l) => Date.parse(String(l.timestamp)));

    // Assert. A back-dated answer preceded its own prompt, which the live
    // stream stamped now; the lapse is the gate's clock's business instead.
    expect(stamps.length).toBeGreaterThan(0);
    expect(stamps.every((stamp) => stamp === HARNESS_NOW_MS)).toBe(true);
  });

  it("leaves a context the cold gate reads as above its floor", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cold-seed"]);
    const facts = readTranscriptFacts(transcriptPath(driven.configDir, driven.cwd, driven.sessionId));

    // Assert
    expect(facts?.contextTokens).toBeGreaterThanOrEqual(COLD_GATE_FLOOR_TOKENS);
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

/**
 * Sample `getContextUsage()` at session start and after every turn.
 *
 * The shim's own cadence is what pushes `context_usage` — at session start and
 * at each turn end, regardless of scenario — so a drift scenario is only
 * testable by asking the same question at those instants and comparing the
 * answers. The wait is a bounded scheduler-yield loop rather than a delay: the
 * mock answers within a few microtasks, and a turn that never ends must fail the
 * test loudly instead of hanging the suite.
 */
async function contextUsageSamples(
  prompts: readonly string[],
): Promise<Record<string, unknown>[]> {
  const samples: Record<string, unknown>[] = [];
  await driveScenario([], {
    during: async (query, feeder, messages) => {
      samples.push(await query.getContextUsage());
      for (const [index, prompt] of prompts.entries()) {
        feeder.push(prompt);
        for (let yields = 0; messages.filter((m) => m.type === "result").length <= index; yields++) {
          if (yields > 10_000) throw new Error(`turn ${index + 1} never produced a result`);
          await new Promise((resolve) => setImmediate(resolve));
        }
        samples.push(await query.getContextUsage());
      }
    },
  });
  return samples;
}

describe("context-usage drift", () => {
  it("answers a DIFFERENT total after the turn than at session start", async () => {
    // Arrange + Act
    const [atStart, afterTurn] = await contextUsageSamples(["!context-usage-drift"]);

    // Assert. The push cadence is the engine's; what this scenario guarantees is
    // that the two samples are distinguishable at all.
    expect(afterTurn?.totalTokens).not.toBe(atStart?.totalTokens);
  });

  it("moves the percentage with the total", async () => {
    // Arrange + Act
    const [atStart, afterTurn] = await contextUsageSamples(["!context-usage-drift"]);

    // Assert
    expect(Number(afterTurn?.percentage)).toBeGreaterThan(Number(atStart?.percentage));
  });

  it("grows the message CATEGORY, not only the total", async () => {
    // Arrange + Act
    const [atStart, afterTurn] = await contextUsageSamples(["!context-usage-drift"]);
    const messages = (sample: Record<string, unknown> | undefined): number =>
      Number(
        ((sample?.categories as { name: string; tokens: number }[]) ?? []).find(
          (c) => c.name === "Messages",
        )?.tokens,
      );

    // Assert
    expect(messages(afterTurn)).toBeGreaterThan(messages(atStart));
  });

  it("grows the messageBreakdown with every further turn", async () => {
    // Arrange + Act
    const samples = await contextUsageSamples(["!context-usage-drift", "!context-usage-drift"]);
    const assistantTokens = samples.map((s) =>
      Number((s.messageBreakdown as { assistantMessageTokens: number }).assistantMessageTokens),
    );

    // Assert. A breakdown that stood still while the total moved would let a
    // consumer that renders it pass against a mock that never changed it.
    expect(assistantTokens[2]).toBeGreaterThan(assistantTokens[1] ?? 0);
  });

  it("leaves the FIXED costs fixed, because a conversation growing does not grow them", async () => {
    // Arrange + Act
    const [atStart, afterTurn] = await contextUsageSamples(["!context-usage-drift"]);
    const systemPrompt = (sample: Record<string, unknown> | undefined): number =>
      Number(
        ((sample?.categories as { name: string; tokens: number }[]) ?? []).find(
          (c) => c.name === "System prompt",
        )?.tokens,
      );

    // Assert
    expect(systemPrompt(afterTurn)).toBe(systemPrompt(atStart));
  });

  it("stays a FULL context-usage answer, field for field", async () => {
    // Arrange + Act
    const [atStart, afterTurn] = await contextUsageSamples(["!context-usage-drift"]);

    // Assert. Drift must not cost the answer a single declared field.
    expect(Object.keys(afterTurn ?? {}).sort()).toEqual(Object.keys(atStart ?? {}).sort());
  });

  it("does NOT drift on an ordinary turn, so the steady case stays testable", async () => {
    // Arrange + Act
    const samples = await contextUsageSamples(["hello", "hello again"]);
    const breakdowns = samples.map((s) => JSON.stringify(s.messageBreakdown));

    // Assert
    expect(new Set(breakdowns).size).toBe(1);
  });
});

describe("an unsolicited model fallback", () => {
  it("announces it with the declared model_refusal_fallback message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);
    const announced = ofType(driven, "system", "model_refusal_fallback")[0];

    // Assert
    expect({
      trigger: announced?.trigger,
      direction: announced?.direction,
      original: announced?.original_model,
      fallback: announced?.fallback_model,
    }).toEqual({
      trigger: "refusal",
      direction: "retry",
      original: "fake-opus-4-8",
      fallback: "fake-sonnet-5",
    });
  });

  it("retracts nothing, because the refusal landed before any block was delivered", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);

    // Assert. EMPTY and not absent: absent would mean an older CLI that cannot
    // say, which is a different fact.
    expect(ofType(driven, "system", "model_refusal_fallback")[0]?.retracted_message_uuids).toEqual([]);
  });

  it("emits the declared session_state_changed beat, which carries no model", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);
    const beat = ofType(driven, "system", "session_state_changed")[0];

    // Assert
    expect({ state: beat?.state, model: beat?.model }).toEqual({ state: "idle", model: undefined });
  });

  it("answers on the FALLBACK model, which is the only evidence the swap happened", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);
    const answers = (driven.messages as unknown as Record<string, unknown>[])
      .filter((m) => m.type === "assistant")
      .map((m) => (m.message as { model: string }).model);

    // Assert. TWO lines, one per block: the closing API response is `[thinking,
    // text]` in every capture and both lines report the model that produced
    // them, which is the fallback.
    expect(answers).toEqual(["fake-sonnet-5", "fake-sonnet-5"]);
  });

  it("STICKS: a following ordinary turn answers on the fallback model too", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback", "and now some prose"]);
    const models = (driven.messages as unknown as Record<string, unknown>[])
      .filter((m) => m.type === "assistant")
      .map((m) => (m.message as { model: string }).model);

    // Assert
    expect(new Set(models)).toEqual(new Set(["fake-sonnet-5"]));
  });

  it("bills the turn to the fallback model", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);

    // Assert
    expect(Object.keys(theResult(driven).modelUsage as Record<string, unknown>)).toEqual([
      "fake-sonnet-5",
    ]);
  });

  it("records the swap on disk as well as on the stream", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!model-fallback"]);
    const record = recordsOfType(driven.transcript(), "system").find(
      (l) => l.subtype === "model_refusal_fallback",
    );

    // Assert
    expect({ original: record?.originalModel, fallback: record?.fallbackModel }).toEqual({
      original: "fake-opus-4-8",
      fallback: "fake-sonnet-5",
    });
  });
});

describe("fast mode as a SESSION fact", () => {
  /** The `init` the mock emits after a rotation — the one that reports fast mode. */
  const initAfterRotation = async (prompt: string): Promise<Record<string, unknown>> => {
    const driven = await driveScenario([prompt, "!rotate"]);
    const inits = ofType(driven, "system", "init");
    const last = inits.at(-1);
    if (last === undefined) throw new Error("no init was emitted");
    return last;
  };

  it("carries the ON state on the rotation's init", async () => {
    // Arrange + Act + Assert. `init` and `result` are the only two places
    // `sdk.d.ts` carries fast mode at all.
    expect((await initAfterRotation("!fast-on")).fast_mode_state).toBe("on");
  });

  it("carries the COOLDOWN state on the rotation's init", async () => {
    // Arrange + Act + Assert
    expect((await initAfterRotation("!fast-cooldown")).fast_mode_state).toBe("cooldown");
  });

  it("carries the vendor's disabled reason on the init when fast mode is OFF", async () => {
    // Arrange + Act
    const init = await initAfterRotation("!fast-off");

    // Assert
    expect({ state: init.fast_mode_state, reason: init.fast_mode_disabled_reason }).toEqual({
      state: "off",
      reason: "preference",
    });
  });

  it("names NO reason on the init while fast mode is on, because nothing blocks it", async () => {
    // Arrange + Act + Assert
    expect((await initAfterRotation("!fast-on")).fast_mode_disabled_reason).toBeUndefined();
  });

  it("STICKS: a later ordinary turn's result reports the state too", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!fast-cooldown", "some ordinary prose"]);
    const states = ofType(driven, "result").map((r) => r.fast_mode_state);

    // Assert
    expect(states).toEqual(["cooldown", "cooldown"]);
  });
});

describe("the named rate-limit windows", () => {
  /** The one `rate_limit_event` a drive produced. */
  const rateLimitInfo = (
    driven: Awaited<ReturnType<typeof driveScenario>>,
  ): Record<string, unknown> =>
    ((driven.messages as unknown as Record<string, unknown>[]).find(
      (m) => m.type === "rate_limit_event",
    )?.rate_limit_info ?? {}) as Record<string, unknown>;

  it("names the five-hour window", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit-five-hour"]);

    // Assert. The footer's window row joins on this and nothing else.
    expect(rateLimitInfo(driven).rateLimitType).toBe("five_hour");
  });

  it("names the seven-day window", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit-seven-day"]);

    // Assert
    expect(rateLimitInfo(driven).rateLimitType).toBe("seven_day");
  });

  it("states a utilization for the five-hour window", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit-five-hour"]);

    // Assert. A window with no utilization draws as a row with no figure.
    expect(rateLimitInfo(driven).utilization).toBe(0.82);
  });

  it("states a seven-day utilization ABOVE the footer's newsworthy gate", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit-seven-day"]);

    // Assert. Below the daemon's DefaultRateLimitNewsworthyThreshold (0.8) the
    // weekly allowance is never drawn at all, so the arm this scenario exists
    // for would be unreachable from it.
    expect(rateLimitInfo(driven).utilization as number).toBeGreaterThan(0.8);
  });

  it("states a reset instant in SECONDS, as the vendor does", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!rate-limit-seven-day"]);

    // Assert. The converter's job is seconds→millis, so the mock must not
    // pre-convert or the conversion would never be exercised.
    const resetsAt = Number(rateLimitInfo(driven).resetsAt);
    expect(resetsAt < 100_000_000_000).toBe(true);
  });

  it("gives the two windows DIFFERENT utilizations, so a join cannot pass by luck", async () => {
    // Arrange + Act
    const five = await driveScenario(["!rate-limit-five-hour"]);
    const seven = await driveScenario(["!rate-limit-seven-day"]);

    // Assert
    expect(rateLimitInfo(five).utilization === rateLimitInfo(seven).utilization).toBe(false);
  });
});
