import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { currentTestName, profileName, PROFILE_DIR_ENV, shimProfileEnv, withoutCpuProf } from "./profile.js";

let root: string;

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "shim-profile-"));
});

afterEach(() => {
  rmSync(root, { recursive: true, force: true });
});

describe("profileName", () => {
  // The same table as testrun/testenv's TestProfileName: a Go harness and this
  // one name a test's directory alike.
  it.each([
    { input: "TestX", want: "TestX" },
    { input: "daemon:internal/merge:TestQueue#0", want: "daemon_internal_merge_TestQueue_0" },
    { input: "TestA/one two//three", want: "TestA_one_two_three" },
    { input: "shim[integration]/a-b.test.ts", want: "shim_integration_a-b.test.ts" },
  ])("names $input as $want", ({ input, want }) => {
    // Act
    const got = profileName(input);

    // Assert
    expect(got).toBe(want);
  });

  it("refuses an empty name", () => {
    // Act
    const act = () => profileName("");

    // Assert
    expect(act).toThrow("never by nothing");
  });
});

describe("withoutCpuProf", () => {
  it.each([
    { input: '--cpu-prof --cpu-prof-dir="/a b/c"', want: "" },
    { input: "--max-old-space-size=99 --cpu-prof --cpu-prof-dir=/x", want: "--max-old-space-size=99" },
    { input: "--cpu-prof-interval=10 --enable-source-maps", want: "--cpu-prof-interval=10 --enable-source-maps" },
    { input: "", want: "" },
  ])("takes the profiler off $input", ({ input, want }) => {
    // Act
    const got = withoutCpuProf(input);

    // Assert
    expect(got).toBe(want);
  });
});

describe("shimProfileEnv", () => {
  it("tells a shim nothing outside a profiling run", () => {
    // Act
    const got = shimProfileEnv({ NODE_OPTIONS: "--cpu-prof" }, "suite > a test");

    // Assert
    expect(got).toEqual({});
  });

  it("gives a shim its test's directory and takes the unit's profiler off", () => {
    // Act
    const got = shimProfileEnv({ [PROFILE_DIR_ENV]: root, NODE_OPTIONS: `--enable-source-maps --cpu-prof --cpu-prof-dir="${root}"` }, "suite > a test");

    // Assert
    const dir = join(root, "suite_a_test");
    expect(got).toEqual({ AGENT_REPL_EXIT_PROFILE_DIR: dir, NODE_OPTIONS: "--enable-source-maps" });
    expect(existsSync(dir)).toBe(true);
  });

  it("drops NODE_OPTIONS that held only the profiler", () => {
    // Act
    const got = shimProfileEnv({ [PROFILE_DIR_ENV]: root, NODE_OPTIONS: "--cpu-prof" }, "t");

    // Assert
    expect(got).toEqual({ AGENT_REPL_EXIT_PROFILE_DIR: join(root, "t"), NODE_OPTIONS: undefined });
  });

  it("refuses a relative directory", () => {
    // Act
    const act = () => shimProfileEnv({ [PROFILE_DIR_ENV]: "rel/dir" }, "t");

    // Assert
    expect(act).toThrow("is not absolute");
  });
});

describe("currentTestName", () => {
  it("names the running test", () => {
    // Act
    const got = currentTestName();

    // Assert
    expect(got).toBe("currentTestName > names the running test");
  });
});
