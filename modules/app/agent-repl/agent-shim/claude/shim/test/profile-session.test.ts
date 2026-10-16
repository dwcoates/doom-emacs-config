/**
 * The owner suite for the shared TypeScript profiling session contract at
 * `agent-shim/logging/ts/profile-session.ts`, asserted against
 * `proto/vocab/profile-session.json`, the contract Go and elisp answer too.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import {
  ACTIVE_FILE,
  ARMED_DIR,
  DEFAULT_ROOT,
  LOG_LEVEL_FILE,
  MAX_SECONDS,
  ROOT_ENV,
  SIGNAL,
  profilesRoot,
  validSession,
} from "../../../logging/ts/profile-session.js";

interface SessionFixture {
  root_env: string;
  default_root: string;
  active_file: string;
  armed_dir: string;
  signal: string;
  max_seconds: number;
  log_level_file: string;
  sessions: { id: string; valid: boolean }[];
}

const fixture = JSON.parse(
  readFileSync(fileURLToPath(new URL("../../../../proto/vocab/profile-session.json", import.meta.url)), "utf8"),
) as SessionFixture;

describe("the shared profiling session contract", () => {
  it("names the fixture's root, files, signal and window", () => {
    // Act
    const got = [ROOT_ENV, `~/${DEFAULT_ROOT}`, ACTIVE_FILE, ARMED_DIR, SIGNAL, MAX_SECONDS, LOG_LEVEL_FILE];

    // Assert
    expect(got).toEqual([
      fixture.root_env,
      fixture.default_root,
      fixture.active_file,
      fixture.armed_dir,
      fixture.signal,
      fixture.max_seconds,
      fixture.log_level_file,
    ]);
  });

  it.each(fixture.sessions)("judges session $id valid=$valid", ({ id, valid }) => {
    // Act
    const got = validSession(id);

    // Assert
    expect(got).toBe(valid);
  });

  it("prefers the override root", () => {
    // Act
    const got = profilesRoot({ [ROOT_ENV]: "/override" }, "/home/u");

    // Assert
    expect(got).toBe("/override");
  });

  it("puts the default root under home", () => {
    // Act
    const got = profilesRoot({}, "/home/u");

    // Assert
    expect(got).toBe("/home/u/.cache/agent-repl/profiles");
  });

  it("refuses a missing home with no override", () => {
    // Act
    const act = () => profilesRoot({}, "");

    // Assert
    expect(act).toThrow(/no home directory/);
  });
});
