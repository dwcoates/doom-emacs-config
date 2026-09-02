import { describe, it, expect, afterEach } from "vitest";
import {
  bundledAgentBinaryVersion,
  recordAgentBinaryVersion,
  requireSessionRuntime,
  resetAgentBinaryVersionForTest,
  runtimeIdentity,
  sdkVersion,
  shimBuildSha,
} from "../src/build-identity.js";

// THE BUILD IDENTITY the daemon's stale-shim refresh compares against its
// dist/.built-sha stamp. The only thing this module must never do is invent a
// value: a fabricated identity makes the daemon either bounce a healthy shim or
// refuse to bounce a stale one.

const original = process.env.SHIM_BUILD_SHA;

afterEach(() => {
  if (original === undefined) delete process.env.SHIM_BUILD_SHA;
  else process.env.SHIM_BUILD_SHA = original;
});

describe("shimBuildSha", () => {
  it("reports the identity the build injected", () => {
    process.env.SHIM_BUILD_SHA = "abc123-dirty";
    expect(shimBuildSha()).toBe("abc123-dirty");
  });

  it("reports an EMPTY identity rather than inventing one when unbuilt", () => {
    delete process.env.SHIM_BUILD_SHA;
    expect(shimBuildSha()).toBe("");
  });
});

describe("sdkVersion", () => {
  it("reads the installed SDK package's own version", async () => {
    // Arrange.
    const { createRequire } = await import("node:module");
    const require = createRequire(import.meta.url);
    const { default: pkg } = await import(
      `${require.resolve("@anthropic-ai/claude-agent-sdk").replace(/[^/]+$/, "")}package.json`,
      { with: { type: "json" } }
    );

    // Act.
    const version = sdkVersion();

    // Assert.
    expect(version).toBe((pkg as { version: string }).version);
  });
});

describe("bundledAgentBinaryVersion", () => {
  it("reads the agent binary version the SDK's bundled manifest declares", () => {
    // Arrange, Act.
    const version = bundledAgentBinaryVersion();

    // Assert.
    expect(version).toMatch(/^\d+\.\d+\.\d+/);
  });
});

describe("recordAgentBinaryVersion", () => {
  afterEach(() => resetAgentBinaryVersionForTest());

  it("lets the live session's report override the packaged manifest", () => {
    // Arrange.
    resetAgentBinaryVersionForTest();

    // Act.
    recordAgentBinaryVersion("9.9.9-from-init");

    // Assert.
    expect(runtimeIdentity().agentBinaryVersion).toBe("9.9.9-from-init");
  });

  it("refuses an empty report rather than recording a sentinel", () => {
    // Arrange, Act, Assert.
    expect(() => recordAgentBinaryVersion("")).toThrow(/empty agent binary version/);
  });
});

describe("requireSessionRuntime", () => {
  afterEach(() => resetAgentBinaryVersionForTest());

  it("populates every non-optional SessionRuntime field once the version is known", () => {
    // Arrange.
    process.env.SHIM_BUILD_SHA = "deadbeef";
    recordAgentBinaryVersion("2.1.220");

    // Act.
    const runtime = requireSessionRuntime();

    // Assert.
    expect(runtime).toEqual({
      shimBuildSha: "deadbeef",
      sdkVersion: sdkVersion(),
      agentBinaryVersion: "2.1.220",
    });
  });
});

describe("the bundle never bakes the build identity", () => {
  it("declares no esbuild substitution for process.env.SHIM_BUILD_SHA", async () => {
    // Arrange. A `define` would replace the expression at BUNDLE time, so the
    // value the daemon exported when it spawned the process would be ignored
    // and a survivor would report the sha of whatever build it came from.
    const { readFileSync } = await import("node:fs");
    const { fileURLToPath } = await import("node:url");
    const buildScript = fileURLToPath(new URL("../build.mjs", import.meta.url));

    // Act.
    const source = readFileSync(buildScript, "utf8");

    // Assert.
    expect(source).not.toMatch(/"process\.env\.SHIM_BUILD_SHA"\s*:/);
  });
});
