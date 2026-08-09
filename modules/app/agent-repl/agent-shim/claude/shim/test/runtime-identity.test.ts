import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { canonicalEncode, canonicalSha256, queryRuntimeIdentity } from "../src/runtime-identity.js";
import type { ShimRuntimeFacts } from "../src/runtime-identity.js";

/** The real SDK `system:init` payload, so a key the SDK does not use cannot pass. */
function corpusInit(): Record<string, unknown> {
  const line = readFileSync(new URL("../../../../testdata/corpus/stream/system_init.jsonl", import.meta.url), "utf8")
    .split("\n")[0]!;
  return JSON.parse(line) as Record<string, unknown>;
}

const shimFacts: ShimRuntimeFacts = {
  vendorSessionId: "vendor-uuid",
  effectiveModel: "claude-opus-5",
  sdkVersion: "0.3.220",
  shimBuildSha: "abc123",
  effectiveQueryOptions: { permissionMode: "default", settingSources: ["user", "project", "local"] },
  contextPrefix: { type: "preset", preset: "claude_code", append: "metaprompt" },
};

describe("canonicalEncode", () => {
  it("orders object keys so insertion order cannot change the digest", () => {
    expect(canonicalEncode({ b: 1, a: 2 })).toBe(canonicalEncode({ a: 2, b: 1 }));
  });

  it("preserves array order, which is part of the configuration", () => {
    expect(canonicalEncode(["a", "b"])).not.toBe(canonicalEncode(["b", "a"]));
  });

  it("encodes a function by shape rather than dropping it", () => {
    expect(canonicalEncode({ canUseTool: function ask() {} })).toContain("[function ask]");
  });

  it("encodes a class instance by its constructor name", () => {
    expect(canonicalEncode(new AbortController())).toBe('"[object AbortController]"');
  });

  it("distinguishes a missing key from a key holding null", () => {
    expect(canonicalEncode({ a: 1 })).not.toBe(canonicalEncode({ a: 1, b: null }));
  });
});

describe("canonicalSha256", () => {
  it("produces a lowercase hexadecimal digest", () => {
    expect(canonicalSha256({ a: 1 })).toMatch(/^[0-9a-f]{64}$/);
  });
});

// Every assertion below is one field of the evidence the daemon's turn
// accounting reconciles against. The defect these cover invalidated 13 of 13
// settled turns across 5 unrelated workspaces: the reader looked for snake_case
// keys the SDK emits in camelCase, so present evidence was recorded as "".
describe("queryRuntimeIdentity", () => {
  it("reads auth_source from the SDK's camelCase apiKeySource", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).authSource).toBe("none");
  });

  it("reads claude_code_version from the SDK's snake_case key", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).claudeCodeVersion).not.toBe("");
  });

  it("reads fast_mode_state from the SDK's snake_case key", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).fastModeState).toBe("off");
  });

  it("reads fast_mode_reason from the SDK's fast_mode_disabled_reason key", () => {
    const init = { ...corpusInit(), fast_mode_disabled_reason: "quota_exhausted" };
    expect(queryRuntimeIdentity(init, shimFacts).fastModeReason).toBe("quota_exhausted");
  });

  it("fingerprints the ordered tool definitions the init message carries", () => {
    const tools = queryRuntimeIdentity(corpusInit(), shimFacts).tools;
    expect(tools?.evidence.case).toBe("sha256");
    expect(tools?.evidence.value).toBe(canonicalSha256(corpusInit()["tools"]));
  });

  it("fingerprints the MCP configuration the init message carries", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).mcp?.evidence.case).toBe("sha256");
  });

  it("fingerprints the effective SDK options the shim passed to query()", () => {
    const options = queryRuntimeIdentity(corpusInit(), shimFacts).effectiveOptions;
    expect(options?.evidence.value).toBe(canonicalSha256(shimFacts.effectiveQueryOptions));
  });

  it("fingerprints the context prefix the shim passed to query()", () => {
    const prefix = queryRuntimeIdentity(corpusInit(), shimFacts).contextPrefix;
    expect(prefix?.evidence.value).toBe(canonicalSha256(shimFacts.contextPrefix));
  });

  it("states a cause rather than hashing absent tools", () => {
    const init = corpusInit();
    delete init["tools"];
    const tools = queryRuntimeIdentity(init, shimFacts).tools;
    expect(tools?.evidence.case).toBe("unavailable");
  });

  it("states why the merged settings cannot be fingerprinted", () => {
    const settings = queryRuntimeIdentity(corpusInit(), shimFacts).settings;
    expect(settings?.evidence.case).toBe("unavailable");
    expect((settings?.evidence.value as { cause: string }).cause).toContain("settingSources");
  });

  it("carries the shim's own vendor session id rather than the init message's", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).vendorSessionId).toBe("vendor-uuid");
  });

  it("leaves subscription_type empty because init never reports it", () => {
    expect(queryRuntimeIdentity(corpusInit(), shimFacts).subscriptionType).toBe("");
  });
});
