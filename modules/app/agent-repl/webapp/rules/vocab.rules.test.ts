/** The render colors' values are the vocabulary file's alone: styles.css declares none of their tokens. */
import { describe, expect, it } from "vitest";
import stylesheet from "../src/styles.css?raw";
import { COLOR_TOKENS, INK_TOKENS } from "../src/vocab.js";
import { rulesOf } from "./stylesheet-text.js";

describe("color_values, each color's one value per theme", () => {
  it.each([...Object.values(COLOR_TOKENS), ...Object.values(INK_TOKENS)])("leaves %s's value to the file: styles.css declares none", (token) => {
    // Arrange
    const declaration = new RegExp(`(?:^|;)\\s*${token}\\s*:`);

    // Act
    const declaredInCss = rulesOf(stylesheet).some((rule) => declaration.test(rule.declarations));

    // Assert
    expect(declaredInCss).toBe(false);
  });
});
