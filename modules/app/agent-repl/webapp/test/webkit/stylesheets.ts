/**
 * The real stylesheets a WebKit suite lays its page out over, read from the
 * source the page and the webhost load: every suite reads them here, so none
 * lays out over a copy that drifted from what ships.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The workspace page's stylesheet (src/styles.css). */
export function pageStylesheet(): Promise<string> {
  return readFile(path.join(here, "../../src/styles.css"), "utf8");
}

/**
 * The webhost's stylesheet: the workspace page's, which the webhost loads too,
 * with the webhost's own (src/host/host.css) after it.
 */
export async function webhostStylesheet(): Promise<string> {
  const [page, host] = await Promise.all([pageStylesheet(), readFile(path.join(here, "../../src/host/host.css"), "utf8")]);
  return `${page}\n${host}`;
}
