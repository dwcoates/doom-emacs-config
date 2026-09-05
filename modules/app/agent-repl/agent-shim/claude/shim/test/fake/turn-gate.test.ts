/**
 * test/fake/turn-gate.test.ts — the turn gate's LOST-EVENT case.
 *
 * The ordinary gate path is covered in index.test.ts, where a real `fs.watch`
 * edge does the waking. This file covers the case that edge never arrives: on
 * macOS FSEvents coalesces and can drop a notification outright, and a gate that
 * trusted the edge alone would then hang forever rather than fail. `fs.watch` is
 * replaced here by a watcher that NEVER fires, so the only thing that can
 * release the turn is the gate's own re-check.
 */
import { describe, expect, it, vi } from "vitest";
import { join } from "node:path";

vi.mock("node:fs", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs")>();
  return {
    ...actual,
    // The suite-wide durable-sink stub, restated because this file's factory
    // replaces the one in test/log-setup.ts rather than composing with it.
    writeSync: vi.fn((_fd: number, _bytes: Buffer, _offset: number, length: number) => length),
    // A watcher that is installed, is closable, and never reports an edge.
    watch: vi.fn(() => ({ close: vi.fn() })),
  };
});

const { TURN_GATE_PATH_ENV, TURN_GATE_TEXT_ENV } = await import("../../src/fake/index.js");
const { driveScenario, theResult } = await import("./harness.js");

describe("the turn gate without its edge", () => {
  it("releases a parked turn when the watch never reports the gate appearing", async () => {
    // Arrange
    const { mkdtempSync, writeFileSync } = await import("node:fs");
    const { tmpdir } = await import("node:os");
    const dir = mkdtempSync(join(tmpdir(), "fake-gate-lost-edge-"));
    const gate = join(dir, "open");
    process.env[TURN_GATE_PATH_ENV] = gate;
    process.env[TURN_GATE_TEXT_ENV] = "gated turn";

    try {
      // Act
      const driven = await driveScenario(["gated turn"], {
        during: async () => {
          // Let the turn reach its park before the gate is written, so the
          // release cannot come from the level check that precedes the watch.
          for (let i = 0; i < 1_000; i++) await new Promise((r) => setImmediate(r));
          writeFileSync(gate, "");
        },
      });

      // Assert
      expect(theResult(driven).subtype).toBe("success");
    } finally {
      delete process.env[TURN_GATE_PATH_ENV];
      delete process.env[TURN_GATE_TEXT_ENV];
    }
  });
});

describe("a parked turn that is killed", () => {
  it("emits its interrupt terminal instead of waiting for a gate that never opens", async () => {
    // Arrange: a gate that is NEVER written. A parked turn is a live turn, and
    // interrupt() is exactly what a consumer sends one that is taking too long
    // -- a gate settling only on the file appearing swallowed the kill, so the
    // shim reported a stopped turn with no terminal behind it.
    const { mkdtempSync } = await import("node:fs");
    const { tmpdir } = await import("node:os");
    const dir = mkdtempSync(join(tmpdir(), "fake-gate-killed-"));
    process.env[TURN_GATE_PATH_ENV] = join(dir, "open");
    process.env[TURN_GATE_TEXT_ENV] = "gated turn";

    try {
      // Act
      const driven = await driveScenario(["gated turn"], {
        during: async (query) => {
          for (let i = 0; i < 1_000; i++) await new Promise((r) => setImmediate(r));
          await query.interrupt?.();
        },
      });

      // Assert
      expect(theResult(driven).subtype).toBe("error_during_execution");
    } finally {
      delete process.env[TURN_GATE_PATH_ENV];
      delete process.env[TURN_GATE_TEXT_ENV];
    }
  });
});
