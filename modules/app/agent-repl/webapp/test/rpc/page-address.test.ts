import { describe, expect, it } from "vitest";
import { pageAddress } from "../../src/rpc/page-address.js";

const BOTH = "?workspace=ws-1&dir=%2Fhome%2Fu%2Fw";

describe("pageAddress", () => {
  it("reads the workspace id", () => {
    expect(pageAddress(BOTH).workspaceId).toBe("ws-1");
  });

  it("URL-decodes the directory", () => {
    expect(pageAddress(BOTH).workspaceDir).toBe("/home/u/w");
  });

  it("URL-decodes an id carrying reserved characters", () => {
    expect(pageAddress("?workspace=a%2Fb%20c&dir=%2Fw").workspaceId).toBe("a/b c");
  });

  it("runs composer-less unless the flag says otherwise", () => {
    expect(pageAddress(BOTH).composer).toBe(false);
  });

  it("enables the dev composer on composer=1", () => {
    expect(pageAddress(`${BOTH}&composer=1`).composer).toBe(true);
  });

  it("uses the logging contract's info level when the host supplies none", () => {
    expect(pageAddress(BOTH).logLevel).toBe("info");
  });

  it("reads the host-delivered AGENT_REPL_LOG_LEVEL value", () => {
    expect(pageAddress(`${BOTH}&log_level=debug`).logLevel).toBe("debug");
  });

  it("reads the end of the delivered level's window", () => {
    expect(pageAddress(`${BOTH}&log_level=debug&log_level_until=1000300`).logLevelUntil).toBe("1000300");
  });

  it("carries no window end when the host supplies none", () => {
    expect(pageAddress(BOTH).logLevelUntil).toBeUndefined();
  });

  it("refuses an unknown host-delivered log level", () => {
    expect(() => pageAddress(`${BOTH}&log_level=trace`)).toThrow(/log_level/);
  });

  it("treats any composer value but 1 as off, rather than as truthy", () => {
    expect(pageAddress(`${BOTH}&composer=true`).composer).toBe(false);
  });

  it("ignores a parameter it does not read, which is somebody else's", () => {
    expect(() => pageAddress(`${BOTH}&cachebust=99`)).not.toThrow();
  });

  it("accepts a search string with no leading question mark", () => {
    expect(pageAddress("workspace=ws-1&dir=/w").workspaceId).toBe("ws-1");
  });

  const missing: ReadonlyArray<[string, string]> = [
    ["an empty search", ""],
    ["only a question mark", "?"],
    ["no workspace", "?dir=%2Fw"],
    ["an empty workspace", "?workspace=&dir=%2Fw"],
    ["no dir", "?workspace=ws-1"],
    ["an empty dir", "?workspace=ws-1&dir="],
  ];
  for (const [label, search] of missing) {
    it(`refuses ${label}, because a page addressed at nothing cannot mount`, () => {
      expect(() => pageAddress(search)).toThrow();
    });
  }

  it("names the missing workspace parameter in the refusal", () => {
    expect(() => pageAddress("?dir=%2Fw")).toThrow(/workspace/);
  });

  it("names the missing dir parameter in the refusal", () => {
    expect(() => pageAddress("?workspace=ws-1")).toThrow(/dir/);
  });
});
