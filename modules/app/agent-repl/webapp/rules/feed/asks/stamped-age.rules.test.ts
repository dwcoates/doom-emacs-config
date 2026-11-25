/**
 * The ask cards draw no hand-rolled age: the cold gate's settled instant goes
 * through stampedAge (src/feed/asks/stamped-age.ts), and the permission and
 * question cards show none of their own.
 */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { join } from "node:path";

describe("stampedAge: the cold gate's own", () => {
  it.each(["permission.ts", "question.ts"])(
    "%s draws no age of its own: its time is the tool card head's clock",
    (file) => {
      // Arrange
      const source = readFileSync(join(__dirname, "../../../src/feed/asks", file), "utf8");
      // Act
      const handRolled = /`\$\{formatTickedAge\([^)]*\)\} ago`/.test(source);
      const stamped = /stampedAge\(/.test(source);
      // Assert
      expect([handRolled, stamped]).toEqual([false, false]);
    },
  );

  it.each(["cold-gate.ts"])(
    "%s draws its settled instant through the shared helper, not a hand-rolled age",
    (file) => {
      // Arrange
      const source = readFileSync(join(__dirname, "../../../src/feed/asks", file), "utf8");
      // Act
      const handRolled = /`\$\{formatTickedAge\([^)]*\)\} ago`/.test(source);
      const shared = /stampedAge\(/.test(source);
      // Assert
      expect([handRolled, shared]).toEqual([false, true]);
    },
  );
});
