/**
 * The spawn gate: one byte opens it, EOF is a daemon that died first.
 *
 * A file stands in for the pipe: what the gate reads is the same either way --
 * a byte, or the end of the stream.
 */
import { closeSync, mkdtempSync, openSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { awaitSpawnGate } from "../src/spawn-gate.js";

/** A readable descriptor whose contents are BODY. */
function gateFd(body: string): number {
  const file = path.join(mkdtempSync(path.join(os.tmpdir(), "spawn-gate-")), "gate");
  writeFileSync(file, body);
  return openSync(file, "r");
}

describe("awaitSpawnGate", () => {
  it("opens on the daemon's byte", async () => {
    await expect(awaitSpawnGate(gateFd("\u0001"))).resolves.toBe("opened");
  });

  it("is abandoned on EOF, a daemon that died before recording the shim", async () => {
    await expect(awaitSpawnGate(gateFd(""))).resolves.toBe("abandoned");
  });

  it("rejects a descriptor it cannot read", async () => {
    const fd = gateFd("");
    closeSync(fd);
    await expect(awaitSpawnGate(fd)).rejects.toThrow();
  });
});
