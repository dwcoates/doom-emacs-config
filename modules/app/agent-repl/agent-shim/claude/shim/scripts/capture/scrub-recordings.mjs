/**
 * Re-scrub committed recordings for the personal values of whoever runs it.
 *
 * `capture.mjs` scrubs every capture it writes, so this exists for recordings
 * made before that scrub existed, and for any recording that reached the tree
 * some other way. It walks each directory named on the command line and, for
 * every file, rewrites the text and the path through `scrubPersonal`. The text
 * is rewritten as text, never re-serialized, so a recording's bytes change only
 * where a personal value stood.
 *
 *   node scrub-recordings.mjs <dir>...
 *
 * The personal values are the host's (see `personalValuesFromHost`), plus
 * $CAPTURE_PERSONAL_NAMES and $CAPTURE_PERSONAL_EMAILS.
 *
 * Node built-ins only, no build step.
 */

import { readFileSync, readdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { scrubPersonal } from "./anonymize.mjs";
import { personalValuesFromHost } from "./capture.mjs";

/**
 * Scrub one tree in place: file contents first, then the entry's own name, so
 * a renamed directory is walked under its old name before it moves. Answers
 * every path it changed, as it stands afterwards.
 */
export function scrubTree(dir, personal, changed = []) {
  for (const name of readdirSync(dir)) {
    const from = path.join(dir, name);
    const info = statSync(from);
    if (info.isDirectory()) {
      scrubTree(from, personal, changed);
    } else if (info.isFile()) {
      const raw = readFileSync(from, "utf8");
      const text = scrubPersonal(raw, personal);
      if (text !== raw) {
        writeFileSync(from, text, "utf8");
        changed.push(from);
      }
    }
    const scrubbed = scrubPersonal(name, personal);
    if (scrubbed !== name) {
      const to = path.join(dir, scrubbed);
      renameSync(from, to);
      changed.push(to);
    }
  }
  return changed;
}

function main(argv, env) {
  if (argv.length === 0) {
    process.stderr.write("usage: scrub-recordings.mjs <dir>...\n");
    return 2;
  }
  const personal = personalValuesFromHost(env);
  for (const dir of argv) {
    const changed = scrubTree(path.resolve(dir), personal);
    process.stdout.write(`scrub-recordings.mjs: ${dir}: ${changed.length} path(s) changed\n`);
  }
  return 0;
}

if (process.argv[1] !== undefined && path.resolve(process.argv[1]) === path.resolve(fileURLToPath(import.meta.url))) {
  process.exit(main(process.argv.slice(2), process.env));
}
