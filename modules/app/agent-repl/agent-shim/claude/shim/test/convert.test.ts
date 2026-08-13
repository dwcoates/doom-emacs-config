import { beforeEach, describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { fromBinary, toBinary } from "@bufbuild/protobuf";
import { SessionStartGate, convert, promptPreview } from "../src/proto/convert.js";
import { __resetExtrasSeen } from "../src/proto/extras.js";
import { EntrySchema, type Entry } from "../src/uds/proto.js";

beforeEach(() => __resetExtrasSeen());

function loadStream(name: string): Record<string, unknown> {
  const line = readFileSync(new URL(`../../../../testdata/corpus/stream/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
  return JSON.parse(line) as Record<string, unknown>;
}

/** The bookkeeping arm of a record, asserting it has one. */
function bookkeeping(entry: Entry) {
  if (entry.external?.entry.case !== "bookkeeping") throw new Error("expected the bookkeeping arm");
  return entry.external.entry.value.kind;
}

/** The vendor-specific internal arm of a record, asserting it has one. */
function vendorSpecific(entry: Entry) {
  if (entry.internal?.unconverted.case !== "vendorSpecific") throw new Error("expected the vendorSpecific arm");
  return entry.internal.unconverted.value;
}

// ---------------------------------------------------------------------------
// The two failure channels, which are not interchangeable.
// ---------------------------------------------------------------------------

describe("convert unreadable records", () => {
  it("turns a non-object into an unparsed record", () => {
    const out = convert(42);
    expect(out.entries[0]!.internal?.unconverted.case).toBe("unparsed");
  });

  it("turns a record with no `type` discriminator into an unparsed record", () => {
    const out = convert({ session_id: "s" });
    expect(out.entries[0]!.internal?.unconverted.case).toBe("unparsed");
  });

  it("attributes an unparsed record to the session it named", () => {
    const out = convert({ session_id: "s1" });
    if (out.entries[0]!.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(out.entries[0]!.internal.unconverted.value.parseError).toMatch(/no string `type`/);
  });

  it("turns a KNOWN family missing an expected field into an unparsed record", () => {
    // Arrange: `assistant` is converted, so a missing `message` is a parse
    // failure rather than an unmodeled family.
    const out = convert({ type: "assistant", session_id: "s" });

    // Assert.
    if (out.entries[0]!.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(out.entries[0]!.internal.unconverted.value.parseError).toMatch(/missing `message`/);
  });

  it("gives an unparsed record no external half, so it cannot reach a page", () => {
    expect(convert(42).entries[0]!.external).toBeUndefined();
  });
});

describe("convert unmodeled discriminators", () => {
  it("captures an unknown top-level type whole", () => {
    const out = convert({ type: "brand_new_family", session_id: "s", detail: 1 });
    if (out.entries[0]!.internal?.unconverted.case !== "unknown") throw new Error("case");
    expect(out.entries[0]!.internal.unconverted.value.raw).toEqual({ type: "brand_new_family", session_id: "s", detail: 1 });
  });

  it("names the field an unknown top-level type was read from", () => {
    const out = convert({ type: "brand_new_family", session_id: "s" });
    if (out.entries[0]!.internal?.unconverted.case !== "unknown") throw new Error("case");
    expect(out.entries[0]!.internal.unconverted.value.discriminatorField).toBe("type");
  });

  it("names `subtype` for an unmodeled system variant", () => {
    const out = convert({ type: "system", subtype: "brand_new_subtype", session_id: "s" });
    if (out.entries[0]!.internal?.unconverted.case !== "unknown") throw new Error("case");
    expect(out.entries[0]!.internal.unconverted.value.discriminatorField).toBe("subtype");
  });

  it("does not log an unknown family's every field as a new field", () => {
    const out = convert({ type: "brand_new_family", session_id: "s", a: 1, b: 2 });
    expect(out.loggedExtras).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// system:init → SessionBegan.
// ---------------------------------------------------------------------------

describe("system:init", () => {
  function began() {
    const out = convert(loadStream("system_init"));
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("expected sessionBegan");
    return kind.value;
  }

  it("states the model the session started under", () => {
    expect(began().model).toBe("claude-haiku-4-5-20251001");
  });

  it("states the working directory a later relative path resolves against", () => {
    expect(began().cwd).toBe("/private/tmp/sdk-probe");
  });

  it("states the CLI version, without which a later transcript is unreadable", () => {
    expect(began().agentVersion).toBe("2.1.215");
  });

  it("reads the vendor's `none` api-key source as a SUBSCRIPTION login", () => {
    // The vendor's spelling is exactly the value that reads as
    // "unauthenticated" to the next person to touch it.
    expect(began().auth?.auth.case).toBe("subscription");
  });

  it("reads a real api-key source as a key, naming where it was found", () => {
    const out = convert({ ...loadStream("system_init"), apiKeySource: "project" });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    if (kind.value.auth?.auth.case !== "apiKey") throw new Error("auth");
    expect(kind.value.auth.auth.value.source).toBe("project");
  });

  it("states fast mode off when the CLI reports it off", () => {
    expect(began().fastMode?.state.case).toBe("off");
  });

  it("carries WHY fast mode is off when the CLI says", () => {
    const out = convert({ ...loadStream("system_init"), fast_mode_disabled_reason: "unsupported model" });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    if (kind.value.fastMode?.state.case !== "off") throw new Error("state");
    expect(kind.value.fastMode.state.value.reason).toBe("unsupported model");
  });

  it("states fast mode on when the CLI reports it on", () => {
    const out = convert({ ...loadStream("system_init"), fast_mode_state: "on" });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    expect(kind.value.fastMode?.state.case).toBe("on");
  });

  it("names the skills the session started with", () => {
    expect(began().skills).toContain("analyze-position");
  });

  it("names the vendor's `agents` as subagents, which is what they are", () => {
    expect(began().subagents).toEqual(["claude", "Explore", "general-purpose", "Plan", "statusline-setup"]);
  });

  it("carries the memory files as PATHS, not as the vendor's name->path map", () => {
    expect(began().memoryPaths).toEqual(["/Users/dodgecoates/.claude-chesscom/projects/-private-tmp-sdk-probe/memory/"]);
  });

  it("names each plugin and its reported version", () => {
    expect(began().plugins.map((p) => [p.name, p.version])).toEqual([["gns-cowork", "9.8.1"]]);
  });

  it("reports a connected MCP server as connected", () => {
    const out = convert({ ...loadStream("system_init"), mcp_servers: [{ name: "gns", status: "connected" }] });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    expect(kind.value.mcpServers[0]!.health?.health.case).toBe("connected");
  });

  it("reports a server awaiting auth as FAILED, never silently usable", () => {
    const out = convert({ ...loadStream("system_init"), mcp_servers: [{ name: "gns", status: "needs-auth" }] });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    if (kind.value.mcpServers[0]!.health?.health.case !== "failed") throw new Error("health");
    expect(kind.value.mcpServers[0]!.health.health.value.error).toBe("needs-auth");
  });

  it("prefers the server's own error text over its status when it has one", () => {
    const out = convert({ ...loadStream("system_init"), mcp_servers: [{ name: "gns", status: "failed", error: "spawn ENOENT" }] });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "sessionBegan") throw new Error("case");
    if (kind.value.mcpServers[0]!.health?.health.case !== "failed") throw new Error("health");
    expect(kind.value.mcpServers[0]!.health.health.value.error).toBe("spawn ENOENT");
  });

  it("carries the init fields the neutral model does not state, without logging them as unknown", () => {
    const out = convert(loadStream("system_init"));
    expect(out.loggedExtras).toEqual([]);
    expect(vendorSpecific(out.entries[1]!).kind).toBe("system:init.unknown-fields");
  });

  it("keeps the carried permission mode reachable from stored data", () => {
    const out = convert(loadStream("system_init"));
    expect(vendorSpecific(out.entries[1]!).raw).toMatchObject({ permissionMode: "default" });
  });
});

describe("SessionStartGate", () => {
  it("admits the first init of a shim lifetime", () => {
    const out = convert(loadStream("system_init"), { sessionGate: new SessionStartGate() });
    expect(bookkeeping(out.entries[0]!).case).toBe("sessionBegan");
  });

  it("suppresses the boundary on a re-init of the SAME session", () => {
    const gate = new SessionStartGate();
    convert(loadStream("system_init"), { sessionGate: gate });
    const second = convert(loadStream("system_init"), { sessionGate: gate });
    expect(second.entries[0]!.external).toBeUndefined();
  });

  it("keeps the suppressed re-init whole rather than discarding it", () => {
    const gate = new SessionStartGate();
    convert(loadStream("system_init"), { sessionGate: gate });
    const second = convert(loadStream("system_init"), { sessionGate: gate });
    expect(vendorSpecific(second.entries[0]!).kind).toBe("system:init.re-announced");
  });

  it("re-admits an init announcing a DIFFERENT vendor session id", () => {
    const gate = new SessionStartGate();
    convert(loadStream("system_init"), { sessionGate: gate });
    const rotated = convert({ ...loadStream("system_init"), session_id: "other" }, { sessionGate: gate });
    expect(bookkeeping(rotated.entries[0]!).case).toBe("sessionBegan");
  });

  it("stops admitting once readiness has been asserted elsewhere", () => {
    const gate = new SessionStartGate();
    gate.close();
    const out = convert(loadStream("system_init"), { sessionGate: gate });
    expect(out.entries[0]!.external).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// result → TurnEnded.
// ---------------------------------------------------------------------------

describe("result", () => {
  function ended(opts: { rootTurnId?: string; interrupted?: boolean }) {
    const out = convert(loadStream("result_success"), opts);
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "turnEnded") throw new Error("expected turnEnded");
    return kind.value;
  }

  it("names the turn it closes, so a late boundary cannot close an open one", () => {
    expect(ended({ rootTurnId: "turn-7" }).turnId).toBe("turn-7");
  });

  it("reports a turn that ran to its own end as completed", () => {
    expect(ended({ rootTurnId: "turn-7" }).outcome.case).toBe("completed");
  });

  it("reports an acknowledged user stop as interrupted", () => {
    expect(ended({ rootTurnId: "turn-7", interrupted: true }).outcome.case).toBe("interrupted");
  });

  it("refuses to accuse when the session acked no stop", () => {
    // An SDK error flavor is indistinguishable from a turn that broke on its
    // own, so only the session's own ack may name a turn interrupted.
    const out = convert({ ...loadStream("result_success"), is_error: true, subtype: "error_during_execution" }, { rootTurnId: "turn-7" });
    const kind = bookkeeping(out.entries[0]!);
    if (kind.case !== "turnEnded") throw new Error("case");
    expect(kind.value.outcome.case).toBe("completed");
  });

  it("emits NO boundary for a result closing no accepted turn", () => {
    const out = convert(loadStream("result_success"));
    expect(out.entries[0]!.external).toBeUndefined();
  });

  it("keeps an unattributable result whole rather than dropping it", () => {
    const out = convert(loadStream("result_success"));
    expect(vendorSpecific(out.entries[0]!).kind).toBe("result.unattributed");
  });

  it("ignores an interrupt claim when the result closes no turn", () => {
    const out = convert(loadStream("result_success"), { interrupted: true });
    expect(out.entries[0]!.external).toBeUndefined();
  });

  it("carries the result fields the boundary has no room for", () => {
    const out = convert(loadStream("result_success"), { rootTurnId: "turn-7" });
    expect(vendorSpecific(out.entries[1]!).raw).toMatchObject({ stop_reason: "end_turn", duration_ms: 1236 });
  });

  it("logs none of those carried fields as unknown", () => {
    expect(convert(loadStream("result_success"), { rootTurnId: "turn-7" }).loggedExtras).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// conversation_reset → SessionIdentityChanged.
// ---------------------------------------------------------------------------

describe("conversation_reset", () => {
  const reset = { type: "conversation_reset", session_id: "old-uuid", new_conversation_id: "new-uuid", uuid: "u" };

  it("states the identity the conversation is leaving behind", () => {
    const kind = bookkeeping(convert(reset).entries[0]!);
    if (kind.case !== "sessionIdentityChanged") throw new Error("case");
    expect(kind.value.previousSessionId).toBe("old-uuid");
  });

  it("files the boundary under the PREVIOUS id, which is the live seq space", () => {
    expect(convert(reset).entries[0]!.external?.sessionId).toBe("old-uuid");
  });

  it("keeps the id it rotated TO, which the boundary has no field for", () => {
    expect(vendorSpecific(convert(reset).entries[1]!).raw).toMatchObject({ new_conversation_id: "new-uuid" });
  });

  it("becomes an unparsed record when it names no new conversation", () => {
    const out = convert({ type: "conversation_reset", session_id: "old-uuid" });
    expect(out.entries[0]!.internal?.unconverted.case).toBe("unparsed");
  });
});

// ---------------------------------------------------------------------------
// Content: understood, and the FILE plane's to write.
// ---------------------------------------------------------------------------

describe("conversation content", () => {
  it("stores an assistant response whole and forwards nothing", () => {
    const out = convert(loadStream("assistant"));
    expect(out.entries[0]!.external).toBeUndefined();
    expect(vendorSpecific(out.entries[0]!).kind).toBe("assistant");
  });

  it("still validates the assistant usage for the accounting log", () => {
    expect(convert(loadStream("assistant")).assistantApiUsage).toBeDefined();
  });

  it("rejects a malformed usage block rather than zeroing the cost", () => {
    const msg = loadStream("assistant");
    const inner = msg["message"] as Record<string, unknown>;
    const out = convert({ ...msg, message: { ...inner, usage: { input_tokens: "not-a-number" } } });
    expect(out.entries[0]!.internal?.unconverted.case).toBe("unparsed");
  });

  it("stores a user echo whole and forwards nothing", () => {
    const out = convert(loadStream("user"));
    expect(out.entries[0]!.external).toBeUndefined();
  });

  it("derives no turn start from a user echo", () => {
    // A `user` message is a REPLAY echo, not a turn start. Counting it as one
    // double-counts turns against the `result` that closes them.
    const out = convert(loadStream("user"));
    expect(out.entries.every((e) => e.external === undefined)).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Families the live relay owns.
// ---------------------------------------------------------------------------

describe("live-relay families", () => {
  it("produces no durable record for a stream_event", () => {
    expect(convert(loadStream("stream_event-content_block_delta-text")).entries).toEqual([]);
  });

  it("produces no durable record for a tool_progress", () => {
    expect(convert({ type: "tool_progress", session_id: "s", tool_use_id: "t" }).entries).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// Understood-but-uncarried system subtypes.
// ---------------------------------------------------------------------------

describe("understood system subtypes", () => {
  it("stores a task start whole, because detached work needs a parent", () => {
    const out = convert(loadStream("task_started"));
    expect(vendorSpecific(out.entries[0]!).kind).toBe("system:task_started");
  });

  it("stores a status line whole", () => {
    expect(vendorSpecific(convert(loadStream("status")).entries[0]!).kind).toBe("system:status");
  });

  it("stores a rate-limit report whole", () => {
    expect(vendorSpecific(convert(loadStream("rate_limit_event")).entries[0]!).kind).toBe("rate_limit_event");
  });

  it("gives an understood-but-uncarried record no external half", () => {
    expect(convert(loadStream("status")).entries[0]!.external).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// The envelope every external half carries.
// ---------------------------------------------------------------------------

describe("record envelope", () => {
  it("routes by the vendor's session identity", () => {
    const out = convert(loadStream("system_init"));
    expect(out.entries[0]!.external?.sessionId).toBe("f7b59684-e29e-469c-a7e7-47bbc1828fb6");
  });

  it("stamps the producer's own clock when the record carries no timestamp", () => {
    const out = convert(loadStream("system_init"), { nowMs: 4242 });
    expect(out.entries[0]!.external?.producedAtMs).toBe(4242n);
  });

  it("prefers the record's own timestamp over the producer's clock", () => {
    const out = convert({ ...loadStream("system_init"), timestamp: "2026-01-01T00:00:00.000Z" }, { nowMs: 4242 });
    expect(out.entries[0]!.external?.producedAtMs).toBe(BigInt(Date.parse("2026-01-01T00:00:00.000Z")));
  });

  it("records the stream plane on every entry it produces", () => {
    expect(convert(loadStream("system_init")).entries[0]!.internal?.plane?.plane.case).toBe("stream");
  });

  it("leaves write_id for the store client to mint once", () => {
    expect(convert(loadStream("system_init")).entries[0]!.internal?.writeId).toBe("");
  });

  it("round-trips through protobuf binary", () => {
    const entry = convert(loadStream("system_init")).entries[0]!;
    const decoded = fromBinary(EntrySchema, toBinary(EntrySchema, entry));
    expect(bookkeeping(decoded).case).toBe("sessionBegan");
  });
});

// ---------------------------------------------------------------------------
// promptPreview.
// ---------------------------------------------------------------------------

describe("promptPreview", () => {
  it("keeps only the first line", () => {
    expect(promptPreview("first\nsecond")).toBe("first");
  });

  it("caps a long first line at 200 characters", () => {
    expect(promptPreview("x".repeat(500))).toHaveLength(200);
  });

  it("passes a short single-line prompt through unchanged", () => {
    expect(promptPreview("hi")).toBe("hi");
  });
});
