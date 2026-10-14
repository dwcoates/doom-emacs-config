/**
 * The detach gate the integration suites park the mocked vendor's detached
 * work on.
 */
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { detachGate } from "./detach-gate.js";

describe("detachGate", () => {
  it("starts closed: its path does not exist", () => {
    // Arrange
    const gate = detachGate();

    // Act
    const open = existsSync(gate.path);

    // Assert
    expect(open).toBe(false);
    gate.dispose();
  });

  it("opens on release: its path exists", () => {
    // Arrange
    const gate = detachGate();

    // Act
    gate.release();

    // Assert
    expect(existsSync(gate.path)).toBe(true);
    gate.dispose();
  });

  it("removes its directory on dispose", () => {
    // Arrange
    const gate = detachGate();
    gate.release();

    // Act
    gate.dispose();

    // Assert
    expect(existsSync(dirname(gate.path))).toBe(false);
  });

  it("gives each gate a directory of its own", () => {
    // Arrange
    const first = detachGate();
    const second = detachGate();

    // Act
    const dirs = [dirname(first.path), dirname(second.path)];

    // Assert
    expect(new Set(dirs).size).toBe(2);
    first.dispose();
    second.dispose();
  });

  it("is the only detach gate the integration suites define", () => {
    // Arrange
    const suites = join(dirname(fileURLToPath(import.meta.url)), "..", "integration");

    // Act
    const handRolled = readdirSync(suites).filter((file) =>
      /function detachGate\(|AGENT_REPL_FAKE_DETACH_GATE: (?!gate\.path)/.test(readFileSync(join(suites, file), "utf8")),
    );

    // Assert
    expect(handRolled).toEqual([]);
  });
});
