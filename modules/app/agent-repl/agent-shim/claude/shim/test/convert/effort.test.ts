import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { effortLevelOf } from "../../src/convert/effort.js";
import { conversationv1 } from "../../src/proto.js";

describe("effortLevelOf", () => {
  it("maps low", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("low")).toBe(conversationv1.AgentEffortLevel.LOW);
  });

  it("maps medium", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("medium")).toBe(conversationv1.AgentEffortLevel.MEDIUM);
  });

  it("maps high", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("high")).toBe(conversationv1.AgentEffortLevel.HIGH);
  });

  it("maps xhigh", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("xhigh")).toBe(conversationv1.AgentEffortLevel.XHIGH);
  });

  it("maps max", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("max")).toBe(conversationv1.AgentEffortLevel.MAX);
  });

  it("is UNSPECIFIED when the tool stated no level", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf(undefined)).toBe(conversationv1.AgentEffortLevel.UNSPECIFIED);
  });

  it("is UNSPECIFIED for a level this vocabulary has no value for, rather than a guess", () => {
    // Arrange, Act, Assert.
    expect(effortLevelOf("ultra")).toBe(conversationv1.AgentEffortLevel.UNSPECIFIED);
  });
});

describe("the effort mapping is shared", () => {
  it("is the only place in src/ that spells a vendor effort level onto the canonical enum", () => {
    // Arrange
    const srcDir = fileURLToPath(new URL("../../src", import.meta.url));
    const home = path.join(srcDir, "convert", "effort.ts");
    const walk = (dir: string): string[] =>
      readdirSync(dir).flatMap((entry) => {
        const full = path.join(dir, entry);
        return statSync(full).isDirectory() ? walk(full) : [full];
      });
    // Act
    const offenders = walk(srcDir).filter(
      (file) =>
        file !== home &&
        !file.includes(`${path.sep}fake${path.sep}`) &&
        file.endsWith(".ts") &&
        /AgentEffortLevel\.XHIGH/.test(readFileSync(file, "utf8")),
    );
    // Assert
    expect(offenders).toEqual([]);
  });
});
