/**
 * The stylesheets every WebKit suite lays out over (stylesheets.ts): the real
 * sources, joined as the webhost loads them, and read nowhere else.
 */
import path from "node:path";
import { readFile, readdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet, webhostStylesheet } from "./stylesheets";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the WebKit suites' stylesheets", () => {
  const seen: { page?: string; webhost?: string; styles?: string; host?: string; handRead?: string[] } = {};

  beforeAll(async () => {
    seen.page = await pageStylesheet();
    seen.webhost = await webhostStylesheet();
    seen.styles = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    seen.host = await readFile(path.join(here, "../../src/host/host.css"), "utf8");
    // EVERY OTHER FILE beside this one, checked for a stylesheet read by hand.
    const files = (await readdir(here)).filter(
      (name) => name.endsWith(".ts") && name !== "stylesheets.ts" && name !== "stylesheets.webkit.test.ts",
    );
    const sources = await Promise.all(files.map(async (name) => [name, await readFile(path.join(here, name), "utf8")] as const));
    seen.handRead = sources.filter(([, source]) => /readFile\([^;]*\.css"/.test(source)).map(([name]) => name);
  });

  it("the page's stylesheet is src/styles.css", () => {
    expect(seen.page).toBe(seen.styles);
  });

  it("the webhost's stylesheet is the page's with src/host/host.css after it", () => {
    expect(seen.webhost).toBe(`${seen.styles}\n${seen.host}`);
  });

  it("no other WebKit suite reads a stylesheet by hand", () => {
    expect(seen.handRead).toEqual([]);
  });
});
