/**
 * The fake store as a process (store-server-main.ts), bundled the way the
 * sidecar's mocked-vendor harness bundles it (build-store-server.mjs).
 */
import { spawn, spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));
let dir = "";
let bundle = "";

beforeAll(() => {
  dir = mkdtempSync(path.join(tmpdir(), "fs-main-"));
  bundle = path.join(dir, "fake-store.mjs");
  const built = spawnSync("node", [path.join(here, "build-store-server.mjs")], {
    env: { ...process.env, STORE_SERVER_OUTFILE: bundle },
    encoding: "utf8",
  });
  expect(built.status, built.stderr).toBe(0);
});

afterAll(() => {
  rmSync(dir, { recursive: true, force: true });
});

/** Spawn the bundle and answer its exit code and stdout once it ends. */
function run(args: string[], onReady?: (child: ReturnType<typeof spawn>) => void): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve) => {
    const child = spawn("node", [bundle, ...args]);
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString();
      if (stdout.includes("ready\n")) onReady?.(child);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      stderr += chunk.toString();
    });
    child.on("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

describe("store-server-main", () => {
  it("serves on the socket, says ready, and closes on SIGTERM", async () => {
    // Arrange.
    const socket = path.join(dir, "s.sock");
    let servedSocket = false;

    // Act.
    const result = await run(["--socket", socket], (child) => {
      servedSocket = existsSync(socket);
      child.kill("SIGTERM");
    });

    // Assert.
    expect(result.stdout).toBe("ready\n");
    expect(servedSocket).toBe(true);
    expect(result.code).toBe(0);
    expect(existsSync(socket)).toBe(false);
  });

  it("refuses to start with no socket named", async () => {
    // Act.
    const result = await run([]);

    // Assert.
    expect(result.code).toBe(2);
    expect(result.stderr).toContain("--socket <path> is required");
  });
});
