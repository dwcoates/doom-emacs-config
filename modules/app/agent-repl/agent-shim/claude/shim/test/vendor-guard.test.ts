import { afterEach, describe, expect, it } from "vitest";
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  FORBID_VENDOR_CALLS_ENV,
  VendorCallsForbiddenError,
  assertVendorCallsAllowed,
  importRealSDK,
} from "../src/vendor-guard.js";
import { makeCreateQuery, parseArgs } from "../src/main.js";

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
  it("never reaches the chokepoint even with the variable set", () => {
    // Arrange
    process.env[FORBID_VENDOR_CALLS_ENV] = "1";
    const createQuery = makeCreateQuery(parseArgs(["--fake", "--session-id", "s1"]));
    const prompt = (async function* () {})() as never;
    const canUseTool = (async () => ({ behavior: "allow" as const, updatedInput: {} })) as never;
    // Act + Assert
    expect(() => createQuery(prompt, canUseTool)).not.toThrow();
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
