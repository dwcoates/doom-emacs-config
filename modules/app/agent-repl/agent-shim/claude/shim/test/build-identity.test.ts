import { describe, it, expect, afterEach } from "vitest";
import { shimBuildSha } from "../src/build-identity.js";

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
