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

  it("carries a login button", () => {
    // Arrange / Act — the topbar markup.
    // Assert
    expect(topbar).toContain(`id="login-btn"`);
  });

  it("places the login button to the right of the permission-mode dropdown", () => {
    // Arrange / Act — source order is layout order in the flex row, and the
    // button is the last control, so it lands at the far right of the topbar.
    // Assert
    expect(topbar.indexOf(`id="login-btn"`)).toBeGreaterThan(topbar.indexOf(`id="mode-select"`));
  });

  it("carries a model picker", () => {
    // Arrange / Act — the topbar markup.
    // Assert
    expect(topbar).toContain(`id="model-select"`);
  });

  it("places the model picker immediately left of the permission-mode dropdown", () => {
    // Arrange / Act — the two session-wide switches sit together, left of
    // login, so the controls that change how a turn RUNS are one group.
    // Assert
    expect(topbar.indexOf(`id="model-select"`)).toBeLessThan(topbar.indexOf(`id="mode-select"`));
  });

  it("keeps the model picker right of the session info block", () => {
    // Arrange / Act — #session-info is pushed right by margin-left:auto, so
    // the picker lands after it rather than adrift on the left.
    // Assert
    expect(topbar.indexOf(`id="model-select"`)).toBeGreaterThan(topbar.indexOf(`id="session-info"`));
  });

  it("starts the model picker empty so its options come only from the daemon", () => {
    // Arrange / Act — unlike #mode-select's fixed enum, the model menu is
    // whatever the account's CLI reports; hardcoding options here would let
    // the picker offer a model the session cannot actually run.
    // Assert
    expect(topbar).toMatch(/<select id="model-select"[^>]*><\/select>/);
  });
});
