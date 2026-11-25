/**
 * THE IMAGE OVERLAY'S OWN RULES IN THE STYLESHEET (`src/styles.css`): the
 * frame inset inside the feed's box, the image as large as fits.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../../src/styles.css?raw";

describe("the image overlay's own rules in the stylesheet", () => {
  const rule = (selector: string): string =>
    stylesheet.match(new RegExp(`${selector.replace(/[[\]"=.-]/g, "\\$&")}\\s*\\{[^}]*\\}`))?.[0] ?? "";

  it("insets the frame inside the feed's box, so the rows show around it", () => {
    expect(rule('[data-component="image-overlay"]')).toContain("padding: 0.75rem 1rem");
  });

  it("draws the image as large as fits, aspect kept", () => {
    expect([rule(".image-overlay-image").includes("max-width: 100%"), rule(".image-overlay-image").includes("object-fit: contain")]).toEqual([
      true,
      true,
    ]);
  });
});
