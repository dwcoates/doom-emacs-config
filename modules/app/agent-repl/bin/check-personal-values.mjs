#!/usr/bin/env node
/**
 * check-personal-values.mjs -- fail when the agent-repl module carries a
 * personal value of whoever runs the check.
 *
 * WHY. agent-repl is run by more than one person, so no file in it may name
 * the person who wrote it: not their home directory, not their account
 * emails, not their name. Per-user values are configuration (see the Setup
 * section of docs/USER-GUIDE.md), and recordings are scrubbed by the capture
 * tooling. This check is what keeps a value from creeping back in.
 *
 * WHAT IS LOOKED FOR. The values the capture scrub uses, with the same
 * matching (agent-shim/claude/shim/scripts/capture/anonymize.mjs's
 * scrubPersonal): the home directory and its vendor project slug, every
 * account email signed in under the usual account roots plus
 * $CAPTURE_PERSONAL_EMAILS, and every name in $CAPTURE_PERSONAL_NAMES. Names
 * are opt-in here, unlike the capture scrub: a login or a git user name can be
 * an ordinary word ("runner", "test") that a source tree rightly contains.
 *
 * WHAT IS SKIPPED. docs/ except docs/USER-GUIDE.md (dated reports are a
 * historical record and are not rewritten), node_modules, dist, coverage and
 * .git directories, binary files, and a skill's `lineage_root:` line (the GNS
 * address a skill was first published under, kept as published).
 *
 * Usage:
 *   bin/check-personal-values.mjs [ROOT]
 *
 *   ROOT  directory to scan (default: the agent-repl module root)
 *
 * Exit status: 0 when nothing is found, 1 when a personal value is found
 * (each one is listed as file:line), 2 on a usage error.
 */
import { readFileSync, readdirSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { scrubPersonal } from "../agent-shim/claude/shim/scripts/capture/anonymize.mjs";
import { envList, personalValuesFromHost } from "../agent-shim/claude/shim/scripts/capture/capture.mjs";

const SKIPPED_DIRS = new Set([".git", "node_modules", "dist", "coverage"]);

/** Every file under root the check reads, as paths relative to root. */
function filesUnder(root, rel = "") {
  const out = [];
  for (const name of readdirSync(path.join(root, rel))) {
    const child = path.join(rel, name);
    const info = statSync(path.join(root, child));
    if (info.isDirectory()) {
      if (SKIPPED_DIRS.has(name)) continue;
      if (child === "docs") {
        out.push(...filesUnder(root, child).filter((f) => f === path.join("docs", "USER-GUIDE.md")));
        continue;
      }
      out.push(...filesUnder(root, child));
    } else if (info.isFile()) {
      out.push(child);
    }
  }
  return out;
}

function main(argv, env) {
  if (argv.length > 1) {
    process.stderr.write(`[check-personal-values] at most one argument (ROOT) is accepted, got ${argv.length}\n`);
    return 2;
  }
  const root = path.resolve(argv[0] ?? path.join(path.dirname(fileURLToPath(import.meta.url)), ".."));
  const host = personalValuesFromHost(env);
  const personal = { home: host.home, emails: host.emails, names: envList(env.CAPTURE_PERSONAL_NAMES) };
  const found = [];
  const files = filesUnder(root);
  for (const rel of files) {
    if (scrubPersonal(rel, personal) !== rel) found.push(`${rel}: the path names a personal value`);
    const buf = readFileSync(path.join(root, rel));
    if (buf.includes(0)) continue;
    buf.toString("utf8").split("\n").forEach((line, i) => {
      if (line.startsWith("lineage_root:")) return;
      if (scrubPersonal(line, personal) !== line) found.push(`${rel}:${i + 1}`);
    });
  }
  if (found.length > 0) {
    process.stderr.write(
      `[check-personal-values] ${found.length} personal value(s) under ${root}:\n` +
        found.map((f) => `  ${f}\n`).join("") +
        "[check-personal-values] make the value configuration (docs/USER-GUIDE.md, Setup), or for a recording run " +
        "agent-shim/claude/shim/scripts/capture/scrub-recordings.mjs over it\n",
    );
    return 1;
  }
  process.stdout.write(`[check-personal-values] ${files.length} files clean under ${root}\n`);
  return 0;
}

process.exit(main(process.argv.slice(2), process.env));
