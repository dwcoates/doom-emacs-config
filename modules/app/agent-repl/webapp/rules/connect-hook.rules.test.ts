/** Every element that waits to join the document is a connect-hook element (src/connect-hook.ts). */
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";

const SRC = join(process.cwd(), "src");

describe("every element that waits to join the document is a connect-hook element", () => {
  const sources = codeUnder(SRC);

  it("defines a custom element nowhere but connect-hook.ts", () => {
    const defining = [...sources].filter(([, code]) => /customElements\.define\(/.test(code)).map(([p]) => p);
    expect(defining).toEqual(["connect-hook.ts"]);
  });

  it.each(["bubble/body.ts", "feed/bubble.ts"])("builds its connect-hook element through the helper in %s", (file) => {
    expect(sources.get(file)).toMatch(/\bcreateConnectHookElement\(/);
  });
});
