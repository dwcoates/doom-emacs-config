/**
 * Topbar markup contract.
 *
 * The session-gone chrome is positional: the remediation notice has to read
 * as an annotation of the alarm dot, which means sitting immediately to its
 * right. jsdom is not in the dep tree, so the ordering is asserted against
 * the document source — which is where the ordering actually lives.
 */
import { describe, expect, it } from "vitest";

import html from "../index.html?raw";

const topbar = html.slice(html.indexOf("<header"), html.indexOf("</header>"));

describe("topbar", () => {
  it("carries a remediation notice slot", () => {
    // Arrange / Act — the topbar markup.
    // Assert
    expect(topbar).toContain(`id="remediation"`);
  });

  it("places the remediation notice to the right of the blinking dot", () => {
    // Arrange / Act — source order is layout order in the flex row.
    // Assert
    expect(topbar.indexOf(`id="remediation"`)).toBeGreaterThan(topbar.indexOf(`id="spinner"`));
  });

  it("keeps the remediation notice left of the session info block", () => {
    // Arrange / Act — #session-info is pushed to the far right by margin-left:auto.
    // Assert
    expect(topbar.indexOf(`id="remediation"`)).toBeLessThan(topbar.indexOf(`id="session-info"`));
  });

  it("starts the notice empty so a healthy session shows no alarm text", () => {
    // Arrange / Act — the notice is filled in only by the gone path.
    // Assert
    expect(topbar).toMatch(/<span id="remediation"><\/span>/);
  });
});
