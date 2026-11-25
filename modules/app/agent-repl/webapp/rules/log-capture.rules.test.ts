/** The one capture (test/log-capture.ts) is the only place a test builds a logger that keeps what it forwards. */
import { readFileSync } from "node:fs";
import { relative, resolve } from "node:path";
import { describe, expect, it } from "vitest";
import { testCodeFiles, webappRoot } from "./files-under.js";

describe("the one capture", () => {
  it("is the only place a test builds a logger that keeps what it forwards", () => {
    // Arrange: log.test.ts is the logger's own suite, which builds loggers
    // directly to test them; every other suite reads records through here.
    const allowed = new Set(["test/log-capture.ts", "test/log.test.ts", "test/log-capture.test.ts", "rules/log-capture.rules.test.ts"]);

    // Act
    const handRolled = testCodeFiles()
      .map((path) => relative(webappRoot, path))
      .filter((path) => !allowed.has(path))
      .filter((path) => /new\s+(\w+\.)?ForwardingLogger\([\s\S]{0,200}?\.push\(record\)/.test(readFileSync(resolve(webappRoot, path), "utf8")));

    // Assert
    expect(handRolled).toEqual([]);
  });
});

