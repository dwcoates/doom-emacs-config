/**
 * The one control (src/control.ts): nothing in src builds a `<button>`, gives
 * an element the button role or disables a row of controls by hand, and the
 * stylesheet styles no native button.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf, withoutBlockComments } from "../test/source-text.js";
import { codeUnder, filesUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

describe("the one control", () => {
  it("is the only way src builds a clickable control: no module creates a <button>", () => {
    // Arrange
    const files = filesUnder(SRC, [".ts", ".html"]).concat([path.join(here, "../index.html")]);
    // Act
    const handRolled = files
      .filter((file) => {
        const code = file.endsWith(".ts") ? codeOf(readFileSync(file, "utf8")) : readFileSync(file, "utf8");
        return /createElement\(\s*["'`]button["'`]\s*\)|<button[\s>]/i.test(code);
      })
      .map((file) => path.relative(path.join(here, ".."), file));
    // Assert
    expect(handRolled).toEqual([]);
  });

  it("is the only way src gives an element the button role", () => {
    // Arrange
    const files = [...codeUnder(SRC)].filter(([file]) => !`${path.sep}${file}`.endsWith(`${path.sep}control.ts`));
    // Act
    const handRolled = files
      .filter(([, code]) => /setAttribute\(\s*["'`]role["'`]\s*,\s*["'`]button["'`]\s*\)/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });

  it("styles no native button: the stylesheet names ar-button, never button", () => {
    // Arrange
    const css = withoutBlockComments(readFileSync(path.join(SRC, "styles.css"), "utf8"));
    // Act
    const selectors = [...css.matchAll(/([^{}]+)\{/g)].map((m) => m[1]);
    const native = selectors.filter((sel) => /(?<![-\w.#:])button(?![-\w])/.test(sel));
    // Assert
    expect(native).toEqual([]);
  });

  it("styles no :disabled control: a control's disabled state is aria-disabled", () => {
    // Arrange
    const css = withoutBlockComments(readFileSync(path.join(SRC, "styles.css"), "utf8"));
    // Act / Assert
    expect(css.includes(":disabled")).toBe(false);
  });
});

describe("labelledControl", () => {
  it.each(["held/held-prompt.ts", "feed/merge/queue-offer.ts", "held/classifier-update.ts"])(
    "is how %s builds every row control",
    (file) => {
      // Arrange
      const code = codeOf(readFileSync(path.join(SRC, file), "utf8"));
      // Act / Assert: a hand-rolled control would call createControl itself.
      expect(code).not.toMatch(/\bcreateControl\(/);
    },
  );
});

describe("whileControlsDisabled", () => {
  it.each(["held/held-prompt.ts", "feed/merge/queue-offer.ts"])("is how %s disables its answers", (file) => {
    const code = codeOf(readFileSync(path.join(SRC, file), "utf8"));
    expect([/\bwhileControlsDisabled\(/.test(code), /querySelectorAll<Control>\(CONTROL_SELECTOR\)/.test(code)]).toEqual([
      true,
      false,
    ]);
  });

  it("is the only way src disables a row of controls", () => {
    // Arrange
    const files = [...codeUnder(SRC)].filter(([file]) => !`${path.sep}${file}`.endsWith(`${path.sep}control.ts`));
    // Act
    const handRolled = files.filter(([, code]) => /\bsetControlsDisabled\(/.test(code)).map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
