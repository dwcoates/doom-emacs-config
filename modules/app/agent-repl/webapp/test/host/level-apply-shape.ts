/**
 * How a source applies a log level that travels between runtimes: the page
 * and the webhost both hand a level and its window's end to
 * `LevelWindow.apply`, and neither opens or closes the window by hand, so the
 * two cannot drift in what an end that has passed, or info, means.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { codeOf } from "../source-text.js";

/** Whether SOURCE goes through `.apply(level, untilMs)`, and whether it also hand-rolls an open or close. */
export function levelApplyShape(source: string): { applies: boolean; handRolls: boolean } {
  const code = codeOf(source);
  return {
    applies: code.includes(".apply(level, untilMs)"),
    handRolls: /\.open\(level,|levelWindow\.close\(\)|env\.level\.close\(\)/.test(code),
  };
}

/** levelApplyShape of the webapp source at SRCPATH, relative to `src/`. */
export function levelApplyShapeOf(srcPath: string): { applies: boolean; handRolls: boolean } {
  return levelApplyShape(readFileSync(join(__dirname, "..", "..", "src", srcPath), "utf8"));
}
