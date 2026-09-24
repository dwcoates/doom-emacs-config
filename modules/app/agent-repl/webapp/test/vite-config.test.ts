// @vitest-environment node
import { existsSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { build, createLogger, type InlineConfig, type Logger, type Rollup } from "vite";
import buildConfig from "../vite.config";

/**
 * THE PRODUCTION BUILD IS CLEAN: it prints no warning, and no chunk it writes
 * is over Vite's 500 kB size advisory.
 *
 * This runs the real `vite build` over vite.config.ts, the config
 * bin/build-frontend.sh ships, into a throwaway directory. Zero warnings is a
 * standing rule, and a warning in a build log is one nobody reads, so the
 * build is held to it here rather than in a scrollback.
 *
 * THE SIZE LIMIT IS VITE'S DEFAULT, NOT A NUMBER OF OURS. The config does not
 * set `build.chunkSizeWarningLimit`, and the first build test pins that:
 * raising it would silence the warning without making any chunk smaller.
 *
 * THE ENTRY REACHES EVERY OTHER CHUNK. bin/build-frontend.sh stamps
 * `dist/.build-id` with the ENTRY bundle's content hash alone, which only
 * fingerprints the whole build because Rollup folds each imported chunk's name
 * (and so its hash) into its importer's hash. A chunk the entry does not reach
 * would change without changing the build id, and a webview would go on
 * answering from its cache.
 *
 * EVERY CHUNK URL IS ORIGIN-ABSOLUTE (`/assets/<name>`). The webview loads the
 * page from `http://ws-<id>.localhost:<port>/`, and the daemon's asset origin
 * (daemon/internal/server/assets.go) serves dist/ at "/" whatever the Host
 * header, so an origin-absolute URL lands on the same daemon under every
 * workspace's host.
 */

const webappDir = path.dirname(fileURLToPath(new URL("../vite.config.ts", import.meta.url)));

// Vite's own default (`build.chunkSizeWarningLimit`), in kB.
const VITE_DEFAULT_CHUNK_LIMIT_KB = 500;

// A healthy build measured ~1.0s alone and ~2.5s with the whole unit suite
// running beside it; ~3x the loaded figure, so a hang still fails.
const BUILD_TIMEOUT_MS = 8000;

type RecordedBuild = { outDir: string; output: Rollup.RollupOutput; warnings: string[] };

/** A logger that records every warning and error instead of printing it. */
function recordingLogger(warnings: string[]): Logger {
  const logger = createLogger("silent");
  logger.warn = (msg) => warnings.push(msg);
  logger.warnOnce = (msg) => warnings.push(msg);
  logger.error = (msg) => warnings.push(msg);
  return logger;
}

/** Runs the real build over vite.config.ts into outDir, recording every warning. */
async function recordBuild(outDir: string, overrides: InlineConfig["build"] = {}): Promise<RecordedBuild> {
  const warnings: string[] = [];
  const result = await build({
    ...buildConfig,
    configFile: false,
    root: webappDir,
    customLogger: recordingLogger(warnings),
    build: { ...buildConfig.build, outDir, emptyOutDir: true, ...overrides },
  });
  if (Array.isArray(result) || !("output" in result)) {
    throw new Error("vite build returned no single RollupOutput; the config grew a second output or a watcher");
  }
  return { outDir, output: result, warnings };
}

/** Every JS chunk in a build output. */
function chunksOf(output: Rollup.RollupOutput): Rollup.OutputChunk[] {
  return output.output.filter((item): item is Rollup.OutputChunk => item.type === "chunk");
}

/** Every JS chunk whose minified code is over limitKb, by file name. */
function oversizedChunks(output: Rollup.RollupOutput, limitKb: number): string[] {
  return chunksOf(output)
    .filter((chunk) => chunk.code.length / 1000 > limitKb)
    .map((chunk) => chunk.fileName);
}

/** The file name of the chunk holding a module whose id contains fragment. */
function chunkHolding(output: Rollup.RollupOutput, fragment: string): string | undefined {
  return chunksOf(output).find((chunk) => chunk.moduleIds.some((id) => id.includes(fragment)))?.fileName;
}

/**
 * The chunk the config's manualChunks names for a module id, called as Rollup
 * would; undefined when it names none (Rollup reads null and undefined alike).
 */
function manualChunkFor(id: string): string | undefined {
  const output = buildConfig.build?.rollupOptions?.output;
  if (output === undefined || Array.isArray(output) || typeof output.manualChunks !== "function") {
    throw new Error("vite.config.ts no longer carries a single output with a manualChunks function");
  }
  const named = output.manualChunks(id, { getModuleIds: () => [][Symbol.iterator](), getModuleInfo: () => null });
  return typeof named === "string" ? named : undefined;
}

describe("vite.config.ts manualChunks", () => {
  const cases: { name: string; id: string; want: string | undefined }[] = [
    { name: "the generated protobuf code goes to proto", id: "/r/proto/gen/ts/agentrepl/v1/api_pb.ts", want: "proto" },
    { name: "the protobuf-es runtime goes to protobuf", id: "/r/node_modules/@bufbuild/protobuf/dist/esm/index.js", want: "protobuf" },
    { name: "the connect client goes to connect", id: "/r/node_modules/@connectrpc/connect-web/dist/esm/index.js", want: "connect" },
    { name: "xterm goes to xterm", id: "/r/node_modules/@xterm/xterm/lib/xterm.js", want: "xterm" },
    { name: "highlight.js goes to highlight", id: "/r/node_modules/highlight.js/lib/core.js", want: "highlight" },
    { name: "markdown-it goes to markdown", id: "/r/node_modules/markdown-it/index.mjs", want: "markdown" },
    { name: "any other vendor module is left to rollup", id: "/r/node_modules/other/index.js", want: undefined },
    { name: "app source is left to rollup", id: "/r/webapp/src/main.ts", want: undefined },
  ];

  it.each(cases)("$name", ({ id, want }) => {
    // Act.
    const got = manualChunkFor(id);

    // Assert.
    expect(got).toBe(want);
  });
});

describe("vite.config.ts build", () => {
  const outDirs: string[] = [];
  let clean: RecordedBuild;

  function tempOutDir(): string {
    const dir = mkdtempSync(path.join(os.tmpdir(), "webapp-build-test-"));
    outDirs.push(dir);
    return dir;
  }

  beforeAll(async () => {
    clean = await recordBuild(tempOutDir());
  }, BUILD_TIMEOUT_MS);

  afterAll(() => {
    for (const dir of outDirs) rmSync(dir, { recursive: true, force: true });
  });

  it("leaves the chunk size warning limit at vite's default rather than raising it", () => {
    // Act.
    const limit = buildConfig.build?.chunkSizeWarningLimit;

    // Assert.
    expect(limit).toBeUndefined();
  });

  it("builds with no warning at all", () => {
    // Act.
    const warnings = clean.warnings;

    // Assert.
    expect(warnings).toEqual([]);
  });

  it("writes no chunk over vite's default size limit", () => {
    // Act.
    const oversized = oversizedChunks(clean.output, VITE_DEFAULT_CHUNK_LIMIT_KB);

    // Assert.
    expect(oversized).toEqual([]);
  });

  it("puts the generated protobuf code in its own chunk, out of the entry", () => {
    // Act.
    const holder = chunkHolding(clean.output, "/proto/gen/ts/");

    // Assert.
    expect(holder).toMatch(/^assets\/proto-[A-Za-z0-9_-]+\.js$/);
  });

  it("builds exactly one entry chunk, the one bin/build-frontend.sh fingerprints", () => {
    // Act.
    const entries = chunksOf(clean.output).filter((chunk) => chunk.isEntry);

    // Assert.
    expect(entries.map((entry) => entry.fileName)).toEqual([expect.stringMatching(/^assets\/index-[A-Za-z0-9_-]+\.js$/)]);
  });

  it("has the entry reach every other chunk, so the entry hash fingerprints the whole build", () => {
    // Arrange.
    const chunks = chunksOf(clean.output);
    const byName = new Map(chunks.map((chunk) => [chunk.fileName, chunk]));
    const reached = new Set<string>();
    const pending = chunks.filter((chunk) => chunk.isEntry).map((chunk) => chunk.fileName);

    // Act.
    for (let name = pending.pop(); name !== undefined; name = pending.pop()) {
      if (reached.has(name)) continue;
      reached.add(name);
      const chunk = byName.get(name);
      pending.push(...(chunk?.imports ?? []), ...(chunk?.dynamicImports ?? []));
    }
    const unreached = chunks.map((chunk) => chunk.fileName).filter((name) => !reached.has(name));

    // Assert.
    expect(unreached).toEqual([]);
  });

  it("resolves every static import of every chunk to a file the build wrote", () => {
    // Act.
    const missing = chunksOf(clean.output).flatMap((chunk) =>
      chunk.imports.filter((name) => !existsSync(path.join(clean.outDir, name))).map((name) => `${chunk.fileName} -> ${name}`),
    );

    // Assert.
    expect(missing).toEqual([]);
  });

  it("resolves every lazy import of every chunk to a file the build wrote", () => {
    // Act.
    const missing = chunksOf(clean.output).flatMap((chunk) =>
      chunk.dynamicImports.filter((name) => !existsSync(path.join(clean.outDir, name))).map((name) => `${chunk.fileName} -> ${name}`),
    );

    // Assert.
    expect(missing).toEqual([]);
  });

  it("keeps xterm a lazy import, loaded only when the login terminal opens", () => {
    // Arrange.
    const xterm = chunkHolding(clean.output, "@xterm/xterm");

    // Act.
    const importers = chunksOf(clean.output).filter((chunk) => xterm !== undefined && chunk.dynamicImports.includes(xterm));

    // Assert.
    expect(importers).not.toEqual([]);
  });

  it("references every script and stylesheet from index.html by an origin-absolute /assets/ url", () => {
    // Arrange.
    const html = readFileSync(path.join(clean.outDir, "index.html"), "utf8");

    // Act.
    const urls = [...html.matchAll(/(?:src|href)="([^"]+\.(?:js|css))"/g)].map((match) => match[1]);

    // Assert.
    expect(urls.filter((url) => !url.startsWith("/assets/"))).toEqual([]);
  });

  it("writes every file index.html references", () => {
    // Arrange.
    const html = readFileSync(path.join(clean.outDir, "index.html"), "utf8");
    const names = [...html.matchAll(/(?:src|href)="\/(assets\/[^"]+)"/g)].map((match) => match[1]);

    // Act.
    const missing = names.filter((name) => !existsSync(path.join(clean.outDir, name)));

    // Assert.
    expect(missing).toEqual([]);
  });

  // THE ERROR PATH: the warning and size checks above must be able to fail.
  // Lowering the limit below every chunk forces Vite's own size warning, and
  // both the recorder and the size scan have to see it.
  describe("when a chunk exceeds the limit", () => {
    const TINY_LIMIT_KB = 1;
    let oversizedBuild: RecordedBuild;

    beforeAll(async () => {
      oversizedBuild = await recordBuild(tempOutDir(), { chunkSizeWarningLimit: TINY_LIMIT_KB });
    }, BUILD_TIMEOUT_MS);

    it("records vite's chunk size warning", () => {
      // Act.
      const warnings = oversizedBuild.warnings;

      // Assert.
      expect(warnings).toEqual([expect.stringContaining("Some chunks are larger than 1 kB after minification")]);
    });

    it("names the oversized chunks in the size scan", () => {
      // Act.
      const oversized = oversizedChunks(oversizedBuild.output, TINY_LIMIT_KB);

      // Assert.
      expect(oversized).toContainEqual(expect.stringMatching(/^assets\/index-[A-Za-z0-9_-]+\.js$/));
    });
  });
});
