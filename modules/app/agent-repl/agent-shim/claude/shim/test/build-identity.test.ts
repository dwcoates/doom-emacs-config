import { describe, it, expect, afterEach, vi } from "vitest";
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
    const { default: pkg } = (await import(
      `${require.resolve("@anthropic-ai/claude-agent-sdk").replace(/[^/]+$/, "")}package.json`,
      { with: { type: "json" } }
    )) as { default: { version: string } };

    // Act.
    const version = sdkVersion();

    // Assert.
    expect(version).toBe((pkg).version);
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

/**
 * What the module does when the SDK's own metadata files are not what it
 * expects.
 *
 * The real installed SDK always has a good package.json and manifest, so the
 * failure arms are unreachable against it. They are driven here by redirecting
 * the module resolution the reader uses, which is the same seam production
 * uses — nothing about the reading logic is stubbed.
 */
describe("reading the SDK's metadata when it is not what it should be", () => {
  const dirs: string[] = [];

  afterEach(async () => {
    vi.doUnmock("node:module");
    vi.resetModules();
    const { rmSync } = await import("node:fs");
    dirs.splice(0).forEach((d) => rmSync(d, { recursive: true, force: true }));
  });

  /** A stand-in SDK package directory holding exactly the files named. */
  async function sdkDirWith(files: Record<string, string>): Promise<string> {
    const { mkdtempSync, writeFileSync } = await import("node:fs");
    const os = await import("node:os");
    const path = await import("node:path");
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-fake-sdk-"));
    dirs.push(dir);
    for (const [name, body] of Object.entries(files)) {
      writeFileSync(path.join(dir, name), body, "utf8");
    }
    return dir;
  }

  /** The build-identity module reading its SDK metadata out of `dir`. */
  async function identityOver(dir: string): Promise<typeof import("../src/build-identity.js")> {
    const path = await import("node:path");
    vi.resetModules();
    vi.doMock("node:module", async (importOriginal) => {
      const actual = await importOriginal<typeof import("node:module")>();
      return {
        ...actual,
        createRequire: (url: string) => {
          const real = actual.createRequire(url);
          const stub = (id: string): unknown => real(id);
          return Object.assign(stub, real, {
            resolve: (id: string): string =>
              id === "@anthropic-ai/claude-agent-sdk" ? path.join(dir, "index.js") : real.resolve(id),
          });
        },
      };
    });
    const log = await import("../src/log.js");
    log.configureLog({ fd: 3, cwd: "/ws", workspaceId: "00000000000000cc", agentReplSessionId: "build-identity-suite" });
    return import("../src/build-identity.js");
  }

  it("refuses to invent an SDK version when the package.json is unreadable", async () => {
    // Arrange: the directory exists but holds no package.json at all.
    const identity = await identityOver(await sdkDirWith({}));

    // Act, Assert — a fabricated version is a lie the daemon cannot detect.
    await expect(async () => identity.sdkVersion()).rejects.toThrow(
      /cannot read the SDK version from/,
    );
  });

  it("refuses an SDK version whose package.json is not valid JSON", async () => {
    // Arrange.
    const identity = await identityOver(await sdkDirWith({ "package.json": "{ not json" }));

    // Act, Assert.
    await expect(async () => identity.sdkVersion()).rejects.toThrow(
      /cannot read the SDK version from/,
    );
  });

  it("refuses an SDK version whose package.json parses to something that is not an object", async () => {
    // Arrange.
    const identity = await identityOver(await sdkDirWith({ "package.json": '"a bare string"' }));

    // Act, Assert.
    await expect(async () => identity.sdkVersion()).rejects.toThrow(
      /cannot read the SDK version from/,
    );
  });

  it("refuses an SDK version the package.json declares as an empty string", async () => {
    // Arrange.
    const identity = await identityOver(await sdkDirWith({ "package.json": '{"version":""}' }));

    // Act, Assert — an empty version is a usable-looking value that says
    // nothing, which is worse than a refusal.
    await expect(async () => identity.sdkVersion()).rejects.toThrow(
      /cannot read the SDK version from/,
    );
  });

  it("reports absence when the SDK ships no manifest to declare a binary version", async () => {
    // Arrange.
    const identity = await identityOver(await sdkDirWith({ "package.json": '{"version":"1.2.3"}' }));

    // Act.
    const version = identity.bundledAgentBinaryVersion();

    // Assert — absence is normal here: system:init carries the value instead.
    expect(version).toBeUndefined();
  });

  it("adopts the manifest's version into the runtime identity when nothing was recorded", async () => {
    // Arrange.
    process.env.SHIM_BUILD_SHA = "cafe123";
    const identity = await identityOver(
      await sdkDirWith({ "package.json": '{"version":"1.2.3"}', "manifest.json": '{"version":"4.5.6"}' }),
    );

    // Act.
    const runtime = identity.runtimeIdentity();

    // Assert.
    expect(runtime).toEqual({
      shimBuildSha: "cafe123",
      sdkVersion: "1.2.3",
      agentBinaryVersion: "4.5.6",
    });
  });

  it("omits the binary version from the runtime identity while nothing has established it", async () => {
    // Arrange.
    const identity = await identityOver(await sdkDirWith({ "package.json": '{"version":"1.2.3"}' }));

    // Act.
    const runtime = identity.runtimeIdentity();

    // Assert — presence, never an "unknown" sentinel.
    expect(runtime.agentBinaryVersion).toBeUndefined();
  });

  it("refuses to build a SessionRuntime while the binary version is still unknown", async () => {
    // Arrange: no manifest, and no session has reported init yet.
    const identity = await identityOver(await sdkDirWith({ "package.json": '{"version":"1.2.3"}' }));

    // Act, Assert.
    await expect(async () => identity.requireSessionRuntime()).rejects.toThrow(
      /agent binary version is not established yet/,
    );
  });
});
