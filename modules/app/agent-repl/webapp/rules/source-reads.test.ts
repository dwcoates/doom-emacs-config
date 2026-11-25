/**
 * A TEST THAT READS THE WEBAPP'S SOURCE IS A RULE, AND RULES LIVE IN `rules/`.
 *
 * The `webapp-rules` suite (this directory) runs before a branch's merge is
 * admitted to the queue, cheaply, because it holds every test that reads the
 * webapp's own source to hold it to a structural rule. A source-reading test
 * left under `test/` would run only in the queue's gate, so this scan fails
 * it, naming the file and the read (`sourceReads`, `source-reads.ts`).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { filesUnder, testDir, webappRoot } from "./files-under.js";
import { sourceReads, type SourceRead } from "./source-reads.js";

/**
 * THE BEHAVIOR READS OF SOURCE, each a file under `test/` and the exact read
 * (`SourceRead.text`) it may make, with why it is no rule: the page's real
 * stylesheet and shell, loaded as the browser loads them so a drawing test
 * runs against what ships, and a runner listing the test files it runs. Every
 * other read of the webapp's source under `test/` is a rule test, which moves
 * to `rules/`.
 */
const BEHAVIOR_READS: readonly { file: string; read: string; reason: string }[] = [
  {
    file: "test/stylesheet.ts",
    read: "../src/styles.css?raw",
    reason: "installs the real stylesheet into jsdom, so a drawing test's cascade is the shipped one",
  },
  {
    file: "test/shell-html.ts",
    read: "../index.html?raw",
    reason: "the page's real shell, the mount points a booting test resolves",
  },
  {
    file: "test/main.test.ts",
    read: 'readFileSync(resolve(process.cwd(), "index.html"), "utf8")',
    reason: "boots main.ts on the page's real shell",
  },
  {
    file: "test/main-boot-logger.test.ts",
    read: 'readFileSync(resolve(process.cwd(), "src/main.ts"), "utf8")',
    reason: "lists the boot's imports to load them before the boot tests' bounds start, so a cold module graph is no boot's cost",
  },
  {
    file: "test/integration/harness.ts",
    read: 'readFileSync(resolve(process.cwd(), "index.html"), "utf8")',
    reason: "mounts the whole app on the page's real shell",
  },
  {
    file: "test/webkit/stylesheets.ts",
    read: 'readFile(path.join(here, "../../src/styles.css"), "utf8")',
    reason: "loads the real page stylesheet into WebKit, so a layout test measures the shipped one",
  },
  {
    file: "test/webkit/stylesheets.ts",
    read: 'readFile(path.join(here, "../../src/host/host.css"), "utf8")',
    reason: "loads the real webhost stylesheet into WebKit, so a layout test measures the shipped one",
  },
  {
    file: "test/webkit/stylesheets.ts",
    read: 'readFile(path.join(here, "../../src/host/sidebar-page.css"), "utf8")',
    reason: "loads the real sidebar page stylesheet into WebKit, so a layout test measures the shipped one",
  },
  {
    file: "test/webapp-layer/layer-server.mjs",
    read: "readdir(LAYER_DIR)",
    reason: "the layer's server lists the layer test files it runs; it reads none of them",
  },
];

/** Whether READ in FILE is one of BEHAVIOR_READS. */
function isBehaviorRead(file: string, read: SourceRead): boolean {
  return BEHAVIOR_READS.some((allowed) => allowed.file === file && allowed.read === read.text);
}

/** Every file under `test/` with the source reads it makes, keyed by its path under the package. */
function testTreeReads(): Map<string, SourceRead[]> {
  return new Map(
    filesUnder(testDir, [".ts", ".tsx", ".mjs"]).map((file) => [path.relative(webappRoot, file), sourceReads(readFileSync(file, "utf8"))]),
  );
}

describe("sourceReads", () => {
  it.each([
    {
      name: "a read of a source file by its path",
      text: 'const source = readFileSync(path.join(here, "../src/feed/feed.ts"), "utf8");',
      want: [{ kind: "reader call", text: 'readFileSync(path.join(here, "../src/feed/feed.ts"), "utf8")' }],
    },
    {
      name: "a listing of a source directory named by a constant",
      text: 'const SRC = path.join(here, "..", "src", "sidebar");\nconst files = readdirSync(SRC);',
      want: [{ kind: "reader call", text: "readdirSync(SRC)" }],
    },
    {
      name: "a read of each path of a mapped list of sources",
      text: 'const sources = await Promise.all(["src/rpc/streams.ts"].map((p) => readFile(p, "utf8")));',
      want: [{ kind: "reader call", text: 'readFile(p, "utf8")' }],
    },
    {
      name: "a read of the test's own file",
      text: 'const self = readFileSync(fileURLToPath(import.meta.url), "utf8");',
      want: [{ kind: "reader call", text: 'readFileSync(fileURLToPath(import.meta.url), "utf8")' }],
    },
    {
      name: "a module glob",
      text: 'const sources = import.meta.glob("../src/**/*.ts", { eager: true });',
      want: [{ kind: "module glob", text: 'import.meta.glob("../src/**/*.ts", { eager: true })' }],
    },
    {
      name: "a raw import of a stylesheet",
      text: 'import css from "../../src/styles.css?raw";',
      want: [{ kind: "raw import", text: "../../src/styles.css?raw" }],
    },
    {
      name: "a dynamic raw import of a source",
      text: 'const loading = import("../src/feed.ts?raw");',
      want: [{ kind: "raw import", text: "../src/feed.ts?raw" }],
    },
    {
      name: "an import of a rules helper",
      text: 'import { codeUnder } from "../../rules/files-under.js";',
      want: [{ kind: "rules helper", text: "../../rules/files-under.js" }],
    },
  ])("finds $name", ({ text, want }) => {
    // Act
    const reads = sourceReads(text);

    // Assert
    expect(reads).toEqual(want);
  });

  it.each([
    {
      name: "a read of a vocabulary file",
      text: 'const vocab = JSON.parse(readFileSync(new URL("../../../proto/vocab/render-colors.json", import.meta.url), "utf8"));',
    },
    {
      name: "a read of a glyph picture",
      text: 'const GLYPHS = path.join(here, "../../images/glyphs");\nconst svg = readFileSync(path.join(GLYPHS, `${name}.svg`), "utf8");',
    },
    {
      name: "a read of a file the test wrote into a temporary directory",
      text: 'const root = mkdtempSync(join(tmpdir(), "x-"));\nconst text = readFileSync(join(root, "a.ts"), "utf8");',
    },
    {
      name: "a read of a build's output",
      text: 'const html = readFileSync(path.join(clean.outDir, "index.html"), "utf8");',
    },
    {
      name: "a reader named in a comment and never called",
      text: "// the scan once called readFileSync on every file",
    },
    {
      name: "a raw import of data",
      text: 'import picture from "../images/glyphs/dot.svg?raw";',
    },
    {
      name: "an import of a test helper",
      text: 'import { installStylesheet } from "../stylesheet.js";',
    },
  ])("finds no read in $name", ({ text }) => {
    // Act
    const reads = sourceReads(text);

    // Assert
    expect(reads).toEqual([]);
  });
});

describe("the webapp's test code under test/", () => {
  const reads = testTreeReads();

  it("reads the webapp's source only to run it, never to hold it to a rule", () => {
    // Act
    const offenders = [...reads]
      .map(([file, found]) => [file, found.filter((read) => !isBehaviorRead(file, read))] as const)
      .filter(([, found]) => found.length > 0)
      .map(([file, found]) => `${file}: ${found.map((read) => `${read.kind} ${read.text}`).join("; ")}`);

    // Assert: each offender is a rule test, which belongs in rules/.
    expect(offenders).toEqual([]);
  });

  it("still makes every behavior read it allows", () => {
    // Act: an allowance whose read is gone would let a new read hide under its name.
    const stale = BEHAVIOR_READS.filter((allowed) => !(reads.get(allowed.file) ?? []).some((read) => read.text === allowed.read)).map(
      (allowed) => `${allowed.file}: ${allowed.read}`,
    );

    // Assert
    expect(stale).toEqual([]);
  });
});
