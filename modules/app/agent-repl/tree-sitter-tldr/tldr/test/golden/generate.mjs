// generate.mjs -- records the webapp's tree parsing as golden data, so the
// tree-sitter port can be held to EXACTLY the same answers.
//
// It ran ONCE, against the webapp's own parser (webapp/src/metaprompt-tree.ts
// at the commit named in golden.json's `source`), before that parser was
// replaced by this grammar. It cannot run again after the replacement: the
// golden data is the frozen behavior every consumer's adapter is tested
// against, not something to regenerate.
//
// Run from this directory:   node generate.mjs
import { build } from "../../../../webapp/node_modules/esbuild/lib/main.js";
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

const webapp = path.resolve("../../../../webapp");
const source = path.join(webapp, "src/metaprompt-tree.ts");
const out = path.resolve(".bundle.mjs");

// The parser's line-level functions are module-private; the bundle exports
// them too, so their answers are recorded rather than reimplemented here.
const internals = [
  "isConnectorLine", "isEmojiRootLine", "isDottedLabelLine", "isHeaderLine",
  "isFenceDelimiter", "parseBranch", "parseContinuation", "splitTreeSegments",
];
await build({
  entryPoints: [source], bundle: true, platform: "node", format: "esm", outfile: out, logLevel: "error",
  plugins: [{
    name: "golden",
    setup(b) {
      b.onResolve({ filter: /\/log\.js$/ }, () => ({ path: "log", namespace: "stub" }));
      b.onLoad({ filter: /.*/, namespace: "stub" }, () => ({ contents: "export const log = { debug(){}, info(){}, warn(){}, error(){} };", loader: "js" }));
      b.onLoad({ filter: /metaprompt-tree\.ts$/ }, (a) => ({
        contents: fs.readFileSync(a.path, "utf8") + `\nexport { ${internals.join(", ")} };\n`, loader: "ts",
      }));
    },
  }],
});
const mp = await import(pathToFileURL(out).href);
fs.rmSync(out);

// --- inputs ---------------------------------------------------------------

/** A seeded PRNG (mulberry32), so the corpus is the same on every run. */
function rng(seed) {
  return () => {
    seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = rng(20261012);
const pick = (xs) => xs[Math.floor(rand() * xs.length)];
const chance = (p) => rand() < p;

const SEGMENTS = ["│   ", "    ", "|   ", "├── ", "└── ", "|-- ", "+-- ", "`-- "];
const STRAYS = ["", "", "", " ", "  ", "\t", " \t"];
const LABELS = ["1", "2", "1.1", "1.2.3", "3.1.2.4", "10", "1.", "1.2.", "3D", "1.2.x", "007"];
const AFTER_LABEL = [" ", " ", "\t", "", "  ", " ", "."];
const MARKERS = ["🔧", "✏️", "👀", "🧑‍💻", "→", "中", "a", "✅", "1"];
const TEXTS = [
  "text", "Changed `foo()` in bar", "<b>bold</b> words", "", "  padded  ", " nbsp lead",
  "ends with cr\r", "trailing tab\t", "emoji 🔧 inside", "1.2 looks like a label", "│ a rail in text",
  "a very long branch body that would certainly need wrapping at narrow widths in the bubble",
];
const PROSE = [
  "plain prose", "1. a markdown list", "1) another list", "| a | b |", "- a bullet", "10 items", "2026 was long",
  "Response (👀 no changes made)", "Response(no space)", " Response (indented)", "│", "│   ", " ", "\t", "   ",
  "├──no space", "└─ short", "|--ascii", "* star", "#", "> quote",
];
const FENCES = ["```", "```go", "│   ```", "    ```", "  ```  go extra", "````", "``` ", "│   ```elisp", " │ ```"];
const HEADERS = ["Response (✏️ changes made)", "Response (👀 no changes made)"];

function branchLine(depth) {
  let p = pick(STRAYS);
  for (let d = 0; d < depth; d++) p += pick(["│   ", "    ", "│   ", "|   "]);
  p += pick(["├── ", "└── ", "├── ", "└── ", "|-- ", "+-- ", "`-- "]);
  const label = chance(0.85) ? pick(LABELS) + pick(AFTER_LABEL) : "";
  return p + label + pick(TEXTS);
}
function rootLine() {
  return pick(LABELS) + pick([" ", " ", " ", "\t", "  ", ""]) + pick(MARKERS) + " " + pick(TEXTS);
}
function randomLine() {
  switch (Math.floor(rand() * 7)) {
    case 0: return branchLine(Math.floor(rand() * 3));
    case 1: return rootLine();
    case 2: return pick(PROSE);
    case 3: return pick(FENCES);
    case 4: return "";
    case 5: return pick(STRAYS) + Array.from({ length: Math.floor(rand() * 3) }, () => pick(SEGMENTS)).join("") + pick(TEXTS);
    default: return pick(LABELS) + pick(AFTER_LABEL) + pick(TEXTS);
  }
}
/** A well-formed response, then sometimes corrupted. */
function wellFormed() {
  const lines = [];
  if (chance(0.7)) lines.push(pick(HEADERS), "");
  if (chance(0.2)) lines.push(pick(PROSE));
  const roots = 1 + Math.floor(rand() * 3);
  for (let r = 1; r <= roots; r++) {
    lines.push(`${r} ${pick(MARKERS)} ${pick(TEXTS)}`);
    const kids = Math.floor(rand() * 3);
    for (let k = 1; k <= kids; k++) {
      const last = k === kids;
      lines.push(`${last ? "└── " : "├── "}${r}.${k} ${pick(TEXTS)}`);
      if (chance(0.3)) {
        const rail = last ? "    " : "│   ";
        lines.push(`${rail}${pick(["```", "```go", "```text"])}`, `${rail}${pick(TEXTS)}`, chance(0.2) ? "" : `${rail}${pick(["```", "```   "])}`);
      }
      const grand = Math.floor(rand() * 2);
      for (let g = 1; g <= grand; g++) lines.push(`${last ? "    " : "│   "}${g === grand ? "└── " : "├── "}${r}.${k}.${g} ${pick(TEXTS)}`);
    }
    if (r < roots && chance(0.8)) lines.push("");
  }
  if (chance(0.3)) lines.push(pick(["", "Trailing prose.", "```", "```go"]), pick(TEXTS));
  for (let c = Math.floor(rand() * 3); c > 0; c--) {
    lines.splice(Math.floor(rand() * (lines.length + 1)), 0, randomLine());
  }
  return lines.join("\n") + (chance(0.5) ? "\n" : "");
}

const inputs = [
  "", "\n", "\n\n", "x", "1 🔧 only root", "1 🔧 a\n└── 1.1 b", "```\n1 🔧 a\n└── 1.1 b\n```",
  "Response (👀 no changes made)\n\n1 🔧 a\n├── 1.1 b\n│   ```go\n│   code\n│   ```\n└── 1.2 c\nafter",
  "1 🔧 a\n└── 1.1 b\n```go\ncode\n```\nprose after", "1 🔧 a\n└── 1.1 b\n```go\ncode\n```\n\n2 ❓ c",
  "1 🔧 a\r\n└── 1.1 b\r\n", "  ├── 1.1 stray\n      ├── 1.2 six", "│   1.2 rails then label\n    1.3 spaces then label",
];
for (let i = 0; i < 1500; i++) inputs.push(wellFormed());
for (let i = 0; i < 1500; i++) {
  inputs.push(Array.from({ length: 1 + Math.floor(rand() * 14) }, randomLine).join("\n") + (chance(0.3) ? "\n" : ""));
}
// A response still arriving: every well-formed sample cut at a random point.
for (let i = 0; i < 600; i++) {
  const whole = wellFormed();
  inputs.push(whole.slice(0, Math.floor(rand() * (whole.length + 1))));
}

// --- answers ----------------------------------------------------------------

/**
 * A region is three runs of the text's lines, so it is recorded as the
 * tree's first line (`top`) and its line count; the before/tree/after strings
 * are those runs rejoined, checked here to expand back exactly.
 */
function compactRegion(region, lines) {
  if (region === null) return null;
  const join = (a, b) => lines.slice(a, b).join("\n");
  for (let top = 0; top <= lines.length; top++) {
    for (let end = top; end <= lines.length; end++) {
      if (join(0, top) === region.before && join(top, end) === region.tree && join(end, lines.length) === region.after) {
        return { top, count: end - top };
      }
    }
  }
  throw new Error("region is not three runs of the text's lines");
}

/**
 * A tree segment's lines are the text's own lines, in order, so it is
 * recorded as its line COUNT; a fence keeps its language and dedented code,
 * plus whether a closing delimiter ended it. The compact form is expanded back
 * and checked against the original here, so it records exactly the same answer.
 */
function compactSegments(segments, lines) {
  const compact = segments.map((s) => (s.kind === "tree" ? { kind: "tree", count: s.lines.length } : { ...s }));
  let i = 0;
  const expanded = compact.map((s) => {
    if (s.kind === "tree") {
      const seg = { kind: "tree", lines: lines.slice(i, i + s.count) };
      i += s.count;
      return seg;
    }
    i += 1 + s.code.length;
    s.closed = i < lines.length;
    if (s.closed) i += 1;
    return { kind: "fence", lang: s.lang, code: s.code };
  });
  if (JSON.stringify(expanded) !== JSON.stringify(segments)) throw new Error("compact segments do not expand back");
  return compact;
}

const cases = inputs.map((text) => ({
  text,
  region: compactRegion(mp.findTreeRegion(text), text.split("\n")),
  isMetapromptTree: mp.isMetapromptTree(text),
  looksLikeIntendedTree: mp.looksLikeIntendedTree(text),
  segments: compactSegments(mp.splitTreeSegments(text.split("\n")), text.split("\n")),
}));

// Each distinct line's answers, recorded once: the same lines recur across
// the cases, and a line's answers depend on the line alone.
const lines = {};
for (const { text } of cases) {
  for (const line of text.split("\n")) {
    lines[line] ??= {
      connector: mp.isConnectorLine(line),
      root: mp.isEmojiRootLine(line),
      dotted: mp.isDottedLabelLine(line),
      header: mp.isHeaderLine(line),
      fence: mp.isFenceDelimiter(line),
      branch: mp.parseBranch(line),
      continuation: mp.parseContinuation(line),
    };
  }
}

const commit = execFileSync("git", ["log", "-1", "--format=%H", "--", source], { encoding: "utf8" }).trim();
fs.writeFileSync("golden.json", JSON.stringify({ source: `webapp/src/metaprompt-tree.ts@${commit}`, cases, lines }) + "\n");
console.log(`${cases.length} cases, ${cases.filter((c) => c.region).length} with a tree region, ${Object.keys(lines).length} distinct lines`);
