import { describe, it, expect } from "vitest";
import indexHtml from "../index.html?raw";
import {
  UNGATED_PERMISSION_MODES,
  effectiveMode,
  isUngatedMode,
  isUngatedSession,
  ungatedBannerHtml,
  ungatedBannerText,
  ungatedModeOf,
  unswitchableModeOptionHtml,
} from "../src/ungated.js";

describe("isUngatedMode", () => {
  it("calls bypassPermissions ungated, the one mode that auto-approves every tool", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("bypassPermissions")).toBe(true);
  });

  it("calls default gated, since canUseTool is consulted for the ask path", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("default")).toBe(false);
  });

  it("calls acceptEdits gated, since non-edit tools still reach canUseTool", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("acceptEdits")).toBe(false);
  });

  it("calls auto gated, since the classifier's ask path still reaches canUseTool", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("auto")).toBe(false);
  });

  it("calls dontAsk gated, because its canUseTool bypass is fail-CLOSED", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("dontAsk")).toBe(false);
  });

  it("calls plan gated, since plan mode executes no mutating tool", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("plan")).toBe(false);
  });

  it("calls an unknown mode gated rather than inventing a verdict for it", () => {
    // Arrange + Act + Assert
    expect(isUngatedMode("someFutureMode")).toBe(false);
  });

  it("names bypassPermissions as the sole ungated mode", () => {
    // Arrange + Act + Assert
    expect([...UNGATED_PERMISSION_MODES]).toEqual(["bypassPermissions"]);
  });
});

describe("effectiveMode", () => {
  it("reads the CLI's own init-reported mode", () => {
    // Arrange
    const init = { permissionMode: "bypassPermissions" };
    // Act + Assert
    expect(effectiveMode(init)).toBe("bypassPermissions");
  });

  it("reports no mode before any init has landed", () => {
    // Arrange + Act + Assert
    expect(effectiveMode(null)).toBe("");
  });

  it("reports no mode when the init carried no permissionMode field", () => {
    // Arrange
    const init = { model: "sonnet" };
    // Act + Assert
    expect(effectiveMode(init)).toBe("");
  });

  it("reports no mode when permissionMode is not a string", () => {
    // Arrange
    const init = { permissionMode: 7 };
    // Act + Assert
    expect(effectiveMode(init)).toBe("");
  });
});

describe("ungatedModeOf", () => {
  it("flags a session LAUNCHED ungated even before its init lands", () => {
    // Arrange
    const args = { requestedMode: "bypassPermissions", systemInit: null };
    // Act + Assert
    expect(ungatedModeOf(args)).toBe("bypassPermissions");
  });

  it("flags a session the CLI reports ungated though the daemon requested default", () => {
    // Arrange: a settings-borne permissions.defaultMode escalation the
    // registry never sees.
    const args = {
      requestedMode: "default",
      systemInit: { permissionMode: "bypassPermissions" },
    };
    // Act + Assert
    expect(ungatedModeOf(args)).toBe("bypassPermissions");
  });

  it("clears a fully gated session", () => {
    // Arrange
    const args = { requestedMode: "default", systemInit: { permissionMode: "default" } };
    // Act + Assert
    expect(ungatedModeOf(args)).toBe("");
  });

  it("keeps flagging when only the requested mode is ungated and the init disagrees", () => {
    // Arrange: over-warning is the safe direction, so one source's silence
    // never suppresses the other's warning.
    const args = {
      requestedMode: "bypassPermissions",
      systemInit: { permissionMode: "default" },
    };
    // Act + Assert
    expect(ungatedModeOf(args)).toBe("bypassPermissions");
  });

  it("answers the boolean question through the same verdict", () => {
    // Arrange
    const args = { requestedMode: "bypassPermissions", systemInit: null };
    // Act + Assert
    expect(isUngatedSession(args)).toBe(true);
  });
});

describe("ungatedBannerText", () => {
  it("names both the absent gate and the mode responsible", () => {
    // Arrange + Act
    const text = ungatedBannerText("bypassPermissions");
    // Assert
    expect(text).toContain("NO PERMISSION GATE");
    expect(text).toContain("bypassPermissions");
  });

  it("says nothing at all for a gated session", () => {
    // Arrange + Act + Assert
    expect(ungatedBannerText("default")).toBe("");
  });
});

describe("ungatedBannerHtml", () => {
  it("renders the mark and the text for an ungated session", () => {
    // Arrange + Act
    const html = ungatedBannerHtml("bypassPermissions");
    // Assert
    expect(html).toContain('class="ungated-mark"');
    expect(html).toContain("NO PERMISSION GATE");
  });

  it("renders empty for a gated session so the slot collapses", () => {
    // Arrange + Act + Assert
    expect(ungatedBannerHtml("default")).toBe("");
  });

  it("offers no dismiss control, since the warning outlives any click", () => {
    // Arrange + Act
    const html = ungatedBannerHtml("bypassPermissions");
    // Assert
    expect(html).not.toContain("<button");
  });
});

describe("unswitchableModeOptionHtml", () => {
  it("carries the live mode as a disabled option when the picker lacks it", () => {
    // Arrange
    const offered = ["default", "acceptEdits", "plan"];
    // Act
    const html = unswitchableModeOptionHtml(offered, "bypassPermissions");
    // Assert
    expect(html).toBe('<option value="bypassPermissions" disabled>bypassPermissions</option>');
  });

  it("adds nothing when the picker already offers the live mode", () => {
    // Arrange
    const offered = ["default", "acceptEdits"];
    // Act + Assert
    expect(unswitchableModeOptionHtml(offered, "default")).toBe("");
  });

  it("adds nothing before any mode is known", () => {
    // Arrange + Act + Assert
    expect(unswitchableModeOptionHtml(["default"], "")).toBe("");
  });

  it("escapes a mode string so an unexpected value cannot inject markup", () => {
    // Arrange + Act
    const html = unswitchableModeOptionHtml(["default"], '<img src=x onerror=1>');
    // Assert
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img");
  });
});

describe("index.html ungated slot", () => {
  it("mounts the banner slot the chrome renders into", () => {
    // Arrange + Act + Assert
    expect(indexHtml).toContain(`<div id="ungated-banner"></div>`);
  });

  it("leaves the slot un-hidden so :empty alone governs its collapse", () => {
    // Arrange / Act — a `hidden` attribute would need a second writer to
    // clear, and a warning gated on two writers is a warning that can be
    // missed.
    // Assert
    expect(indexHtml).not.toContain(`<div id="ungated-banner" hidden>`);
  });

  it("keeps the slot out of the topbar so the picker layout is untouched", () => {
    // Arrange + Act
    const headerEnd = indexHtml.indexOf("</header>");
    // Assert
    expect(indexHtml.indexOf(`id="ungated-banner"`)).toBeGreaterThan(headerEnd);
  });

  it("omits bypassPermissions from the switchable picker it is launch-only for", () => {
    // Arrange / Act — the CLI rejects a mid-session switch into it, so the
    // banner, never an option, is how the mode surfaces.
    // Assert
    expect(indexHtml).not.toContain(`<option value="bypassPermissions">`);
  });
});
