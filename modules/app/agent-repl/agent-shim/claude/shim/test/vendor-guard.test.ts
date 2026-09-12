import { afterEach, describe, expect, it } from "vitest";
import { mkdtempSync, readdirSync, readFileSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  FORBID_VENDOR_CALLS_ENV,
  VendorCallsForbiddenError,
  assertVendorCallsAllowed,
  importRealSDK,
} from "../src/vendor-guard.js";
import { createFakeQuery } from "../src/fake/index.js";
import { driveScenario } from "./fake/harness.js";

// test/setup.ts sets the variable for the whole suite; the "allowed" cases
// below clear it and this restores the suite-wide posture afterwards.
afterEach(() => {
  process.env[FORBID_VENDOR_CALLS_ENV] = "1";
});

describe("assertVendorCallsAllowed", () => {
  it("throws a VendorCallsForbiddenError when the variable is set", () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";
    // Act + Assert
    expect(() => assertVendorCallsAllowed("site-under-test")).toThrow(VendorCallsForbiddenError);
  });

  it("names the variable, the test-mode posture, and the blocked site", () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";
    // Act + Assert
    expect(() => assertVendorCallsAllowed("site-under-test")).toThrow(
      /AGENT_REPL_FORBID_VENDOR_CALLS is set: the shim is running in test mode and real Claude Agent SDK calls are forbidden \(blocked at: site-under-test\)/,
    );
  });

  it("treats any non-empty value as forbidding", () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "0";
    // Act + Assert
    expect(() => assertVendorCallsAllowed("site")).toThrow(VendorCallsForbiddenError);
  });

  it("permits the call when the variable is unset", () => {
    // Arrange
    delete process.env[FORBID_VENDOR_CALLS_ENV];
    // Act + Assert
    expect(() => assertVendorCallsAllowed("site")).not.toThrow();
  });

  it("permits the call when the variable is set but empty", () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "";
    // Act + Assert
    expect(() => assertVendorCallsAllowed("site")).not.toThrow();
  });
});

describe("importRealSDK", () => {
  it("rejects before loading the vendor module when the variable is set", async () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";
    // Act + Assert
    await expect(importRealSDK("chokepoint")).rejects.toThrow(VendorCallsForbiddenError);
  });
});

describe("fake mode", () => {
  it("builds a query without ever reaching the vendor chokepoint", () => {
    // Arrange. The mocked vendor is a full scenario engine now, so `--fake`
    // must SUCCEED with the guard armed. A VendorCallsForbiddenError here would
    // mean --fake had reached for the real SDK; any other throw would mean the
    // mock cannot run offline, which is the one thing it exists to do.
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";
    const prompt = (async function* () {})() as never;
    const canUseTool = (async () => ({ behavior: "allow" as const, updatedInput: {} })) as never;

    // Act.
    let raised: unknown;
    let built = false;
    try {
      const query = createFakeQuery(prompt, canUseTool, {
        sessionId: "s1",
        newUuid: () => "u1",
        cwd: mkdtempSync(path.join(tmpdir(), "vendor-guard-fake-")),
        configDir: mkdtempSync(path.join(tmpdir(), "vendor-guard-cfg-")),
        spoolRoot: mkdtempSync(path.join(tmpdir(), "vendor-guard-spool-")),
      });
      built = true;
      query.close();
    } catch (err) {
      raised = err;
    }

    // Assert.
    expect({
      built,
      forbidden: raised instanceof VendorCallsForbiddenError,
      message: raised instanceof Error ? raised.message : JSON.stringify(raised) ?? "",
    }).toEqual({ built: true, forbidden: false, message: "" });
  });

  it("SERVES a whole turn under the guard, not merely constructs a query", async () => {
    // Arrange. The daemon now spawns every shim in fake mode when it is itself
    // under the guard, so a guarded run's workspaces are created, forked and
    // PROMPTED against this engine. Constructing the query is not enough for
    // that: the turn has to be answered end to end with the guard armed.
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";

    // Act.
    const driven = await driveScenario(["hello"]);

    // Assert.
    expect({
      forbidden: driven.failure instanceof VendorCallsForbiddenError,
      results: driven.messages.filter((m) => m.type === "result").length,
    }).toEqual({ forbidden: false, results: 1 });
  });
});

describe("the chokepoint is structural", () => {
  it("is the only dynamic import of the vendor SDK in src/", () => {
    // Arrange
    const srcDir = fileURLToPath(new URL("../src", import.meta.url));
    const guard = path.join(srcDir, "vendor-guard.ts");
    const walk = (dir: string): string[] =>
      readdirSync(dir).flatMap((entry) => {
        const full = path.join(dir, entry);
        return statSync(full).isDirectory() ? walk(full) : [full];
      });
    // Act
    const offenders = walk(srcDir).filter(
      (file) =>
        file !== guard &&
        file.endsWith(".ts") &&
        /import\s*\(\s*["']@anthropic-ai\/claude-agent-sdk["']/.test(readFileSync(file, "utf8")),
    );
    // Assert
    expect(offenders).toEqual([]);
  });
});
