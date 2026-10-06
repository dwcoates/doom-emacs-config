/**
 * TOPBAR — the thin strip and its reveals, plus the login terminal it opens.
 *
 * Three contracts meet here. The strip itself is ordinary server-driven
 * drawing. The pickers are RPC echoes: a model pick echoes the served
 * `AgentModel` and a mode pick echoes the option's own `mode` string, because
 * the client must never construct an identity it was handed. And the account
 * cell is the door to the login pty — by way of its OPTIONS dropdown, since a
 * pick is what calls SelectAccount and a picked root with no login is what
 * opens OpenLogin (owner ruling, 2026-09-13) — whose whole lifecycle
 * (OpenLogin, WatchLoginTerminal's scrollback, SendLoginInput's two arms,
 * `closed`, CloseLogin) is asserted end to end.
 *
 * Reveals open BELOW the strip by ruling, so the geometry is asserted too, not
 * just the presence of the element.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";

import {
  TopbarAccountSchema,
  TopbarWarningSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { SetModelResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import { SetPermissionModeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_permission_mode_pb";

import { pressurePercentColor } from "../../src/pressure-color.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { bootColdOnce, chipFailureText, startHarness, type Harness } from "./harness";
import { MODEL_PLACEHOLDER, MODEL_TOOLTIP } from "../../src/topbar/model";
import { EFFORT_TOOLTIP } from "../../src/topbar/effort";
import { AgentEffortLevel } from "../../../proto/gen/ts/conversation/v1/api_pb";
import { isKnownTone, RENDER_COLORS } from "./vocab";
import {
  ACCOUNT_CONFIG_DIR,
  OTHER_ACCOUNT_CONFIG_DIR,
  LIVE_DEFAULT_MODE,
  EFFORT_LEVELS,
  PERMISSION_MODES,
  TOPBAR_ACCOUNT_ARMS,
  TOPBAR_WARNING_ARMS,
  WORKSPACE_ID,
  assertCoversOneof,
  degradedWindowClosedWarning,
  topbarView,
  topbarWarning,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one topbar view already scripted. */
async function withTopbar(init: Parameters<typeof topbarView>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView(init)) });
  return harness;
}

describe("arm coverage", () => {
  it("covers every account arm", () => {
    assertCoversOneof(TopbarAccountSchema, "state", [...TOPBAR_ACCOUNT_ARMS]);
  });

  it("covers every warning detail arm", () => {
    assertCoversOneof(TopbarWarningSchema, "detail", [...TOPBAR_WARNING_ARMS]);
  });

  it("lists every tone the vocabulary permits", () => {
    // Assert: the topbar validates `tone` against this list, so it must exist.
    expect(RENDER_COLORS.topbar_tones.length).toBeGreaterThan(0);
  });
});

describe("the connectivity glyph's session dropdown", () => {
  const SESSION = {
    startedAtMs: BigInt(Date.now() - 2 * 60 * 60_000),
    began: "login" as const,
  };

  it("opens agent-repl's session below the strip from the glyph", async () => {
    // Arrange
    await withTopbar({ agentReplSession: SESSION });
    // Act
    await harness.click(".topbar-connectivity");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="agent-repl-session"] .topbar-session-duration .topbar-session-value')).not.toBe(
      "",
    );
  });

  it("states what began the session", async () => {
    // Arrange
    await withTopbar({ agentReplSession: SESSION });
    // Act
    await harness.click(".topbar-connectivity");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="agent-repl-session"] .topbar-session-duration')).toContain(
      "since login",
    );
  });

  it("carries a newer push's session into the dropdown the reader has open", async () => {
    // Arrange
    await withTopbar({ agentReplSession: SESSION });
    await harness.click(".topbar-connectivity");
    // Act
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ agentReplSession: { ...SESSION, began: "editorStart" } }));
    await harness.settle();
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="agent-repl-session"] .topbar-session-duration')).toContain(
      "since Emacs started",
    );
  });

  it("opens the account options from a glyph that carries no session", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-connectivity");
    // Assert
    expect(harness.$(".topbar-reveal")?.getAttribute("data-reveal")).toBe("account");
  });
});

describe("the title and session line", () => {
  it("draws the served title verbatim", async () => {
    // Arrange / Act
    await withTopbar({ title: "port the webapp" });
    // Assert
    expect(harness.text(".topbar-title")).toBe("port the webapp");
  });

  it("reveals the session line below the strip", async () => {
    // Arrange
    await withTopbar({ sessionLine: "session 3 of the overhaul" });
    // Act
    await harness.click(".topbar-title");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="session"]')).toContain(
      "session 3 of the overhaul",
    );
  });

  // A LONG ACCOUNT ROOT IS THE ORDINARY CASE, not a stress case: the line the
  // daemon composes is `<session id> · <account root> · <model>` and a checkout
  // path is as long as the checkout is deep. Drawn `nowrap` inside a panel
  // capped at `min(90vw, 32rem)`, the MODEL at the end of it was off the glass
  // in every capture -- and the model is exactly what a reader opens this
  // reveal to check.
  it("keeps the whole line on the glass when the account root is long", async () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      await withTopbar({
        sessionLine: "vend-7f3a91 · ~/workspace/chesscom/explanation-engine/worktrees/overhaul · fake-opus-4-8",
      });

      // Act
      await harness.click(".topbar-title");

      // Assert: the model is drawn, and the line is free to take a second row
      // rather than run past the panel's cap with its tail hidden.
      const line = harness.$('.topbar-reveal[data-reveal="session"] .topbar-session-line');
      if (!line) throw new Error("the topbar drew no session line in the reveal");
      expect(line.textContent).toContain("fake-opus-4-8");
      expect(cascadedValue(line, "white-space")).not.toBe("nowrap");
    } finally {
      teardown();
    }
  });

  it("opens the session reveal downward, below the strip", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-title");
    // Assert: reveals render BELOW their strip by ruling, so the reveal must
    // FOLLOW the strip row in document order rather than precede it.
    const strip = harness.$("[data-topbar-strip]");
    const reveal = harness.$('.topbar-reveal[data-reveal="session"]');
    if (!strip || !reveal) throw new Error("the topbar drew no strip row or no session reveal");
    const relation = strip.compareDocumentPosition(reveal);
    expect(relation & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  });

  it("carries the rotated session identity into a reveal the reader already had open", async () => {
    // Arrange: the reader opens the session reveal on the session the page
    // booted with. This is the state a rotation actually finds a reader in,
    // and it is the state a real page shows.
    await withTopbar({ sessionLine: "vend-1 · ~/.claude · fake-opus-4-8" });
    await harness.click(".topbar-title");
    // Act: the vendor rotated its conversation, so the daemon pushes a whole
    // new topbar view carrying the NEW identity.
    harness.fake.setTopbar(
      WORKSPACE_ID,
      topbarView({ sessionLine: "vend-2 · ~/.claude · fake-opus-4-8" }),
    );
    await harness.settle();
    // Assert: the panel is still open and speaks for the new session. A push
    // replaces the strip WHOLE, so a reveal that did not re-draw from the new
    // push would leave the reader reading an identity that no longer exists.
    expect(harness.text('.topbar-reveal[data-reveal="session"]')).toContain("vend-2");
  });

  it("does not leave the pre-rotation session identity in the open reveal", async () => {
    // Arrange
    await withTopbar({ sessionLine: "vend-1 · ~/.claude · fake-opus-4-8" });
    await harness.click(".topbar-title");
    // Act
    harness.fake.setTopbar(
      WORKSPACE_ID,
      topbarView({ sessionLine: "vend-2 · ~/.claude · fake-opus-4-8" }),
    );
    await harness.settle();
    // Assert: the OLD id is gone, which is the half "contains the new one"
    // cannot prove on its own -- a panel drawing both lines would pass that.
    expect(harness.text('.topbar-reveal[data-reveal="session"]')).not.toContain("vend-1");
  });

  it("does not open the session reveal above the strip", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-title");
    // Assert
    const strip = harness.$("[data-topbar-strip]");
    const reveal = harness.$('.topbar-reveal[data-reveal="session"]');
    if (!strip || !reveal) throw new Error("the topbar drew no strip row or no session reveal");
    expect(strip.compareDocumentPosition(reveal) & Node.DOCUMENT_POSITION_PRECEDING).toBe(0);
  });
});

describe("the account chip", () => {
  it("draws the email when logged in", async () => {
    // Arrange / Act
    await withTopbar({ account: "loggedIn", email: "dev@example.test" });
    // Assert
    expect(harness.text(".topbar-account")).toContain("dev@example.test");
  });

  it("carries the logged-in arm", async () => {
    // Arrange / Act
    await withTopbar({ account: "loggedIn" });
    // Assert
    expect(harness.$(".topbar-account")?.dataset.arm).toBe("loggedIn");
  });

  it("carries the logged-out arm", async () => {
    // Arrange / Act
    await withTopbar({ account: "loggedOut" });
    // Assert
    expect(harness.$(".topbar-account")?.dataset.arm).toBe("loggedOut");
  });

  it("draws logged out as a warning state", async () => {
    // Arrange / Act
    await withTopbar({ account: "loggedOut" });
    // Assert
    expect(harness.$(".topbar-account")?.className).toMatch(/warn/);
  });

  it("does not draw the logged-in chip as a warning", async () => {
    // Arrange / Act
    await withTopbar({ account: "loggedIn" });
    // Assert
    expect(harness.$(".topbar-account")?.className ?? "").not.toMatch(/warn/);
  });
});

describe("connectivity", () => {
  it("draws the served tone verbatim", async () => {
    // Arrange / Act
    await withTopbar({ tone: "blue" });
    // Assert
    expect(harness.$(".topbar-connectivity")?.dataset.tone).toBe("blue");
  });

  it("draws the served glyph verbatim", async () => {
    // Arrange / Act
    await withTopbar({ glyph: "dot" });
    // Assert
    expect(harness.$(".topbar-connectivity")?.dataset.glyph).toBe("dot");
  });

  it("draws the served title verbatim", async () => {
    // Arrange / Act
    await withTopbar({ connectivityTitle: "connected to claude-repld" });
    // Assert
    expect(harness.$(".topbar-connectivity")?.getAttribute("title")).toBe(
      "connected to claude-repld",
    );
  });

  it.each(RENDER_COLORS.topbar_tones)("accepts the %s tone the vocabulary declares", async (tone) => {
    // Arrange / Act
    await withTopbar({ tone });
    // Assert
    expect(harness.$(".topbar-connectivity")?.dataset.tone).toBe(tone);
  });

  it("refuses a tone the vocabulary does not declare", async () => {
    // Arrange / Act
    await withTopbar({ tone: "chartreuse" });
    // Assert: an unknown tone is a malformed view, not a color to invent.
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("agrees with the vocabulary about which tones are known", () => {
    // Assert: guards this suite's own helper against the file.
    expect(isKnownTone("chartreuse")).toBe(false);
  });
});

describe("the model selector", () => {
  it("draws the selected option's display name", async () => {
    // Arrange / Act
    await withTopbar({});
    // Assert
    expect(harness.text(".topbar-model")).toContain("Opus");
  });

  it("names the model in force by the served AgentModel, not by the label drawn", async () => {
    // Arrange / Act: the pushed selection is `sonnet` while the chip's text is
    // the catalog's first display name, so only the echo token identifies it.
    await withTopbar({ selected: "sonnet" });
    // Assert
    expect(harness.$(".topbar-model")?.getAttribute("data-model")).toBe("sonnet");
  });

  it("marks the offered row that the served selection names", async () => {
    // Arrange
    await withTopbar({ selected: "sonnet" });
    // Act
    await harness.click(".topbar-model");
    // Assert
    expect(
      harness.$$("[data-model-option][data-selected]").map((el) => el.dataset.modelOption),
    ).toEqual(["sonnet"]);
  });

  it("lists exactly the options the view carries", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-model");
    // Assert
    expect(harness.$$("[data-model-option]").map((el) => el.dataset.modelOption)).toEqual([
      "opus",
      "sonnet",
    ]);
  });

  it("draws each option's description verbatim", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-model");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="model"]')).toContain("the fast one");
  });

  it("calls SetModel echoing the served AgentModel", async () => {
    // Arrange
    await withTopbar({});
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    const [request] = harness.fake.calls<{ model?: { name: string } }>("setModel");
    expect(request.model?.name).toBe("sonnet");
  });

  it("echoes the workspace on the model call", async () => {
    // Arrange
    await withTopbar({});
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("setModel");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("draws the refusal at the picker", async () => {
    // Arrange: a TYPED cause, because since landing 4 every `<Rpc>Error`
    // carries one — an error whose cause oneof is unset is a malformed view,
    // which the test below asserts separately.
    await withTopbar({});
    harness.fake.refuse("setModel", "notInCatalog");
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.$(".topbar-model .refusal")).not.toBeNull();
  });

  it("carries the refused arm at the picker", async () => {
    // Arrange
    await withTopbar({});
    harness.fake.refuse("setModel", "vendorRefused");
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.$(".topbar-model .refusal")?.dataset.arm).toBe("vendorRefused");
  });

  it("reports a malformed view for an error with no cause", async () => {
    // Arrange
    await withTopbar({});
    harness.fake.answer(
      "setModel",
      create(SetModelResponseSchema, { result: { case: "error", value: {} } }),
    );
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("draws no refusal for an error with no cause", async () => {
    // Arrange: inventing a sentence would state a refusal the daemon never made.
    await withTopbar({});
    harness.fake.answer(
      "setModel",
      create(SetModelResponseSchema, { result: { case: "error", value: {} } }),
    );
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.$(".topbar-model .refusal")).toBeNull();
  });

  it("opens the model reveal downward", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-model");
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="model"]')).not.toBeNull();
  });
});

describe("the effort selector", () => {
  it("is drawn between the model selector and the permission-mode picker", async () => {
    // Arrange / Act
    await withTopbar({ effort: "medium" });
    // Assert
    const right = [...(harness.$(".topbar-right")?.children ?? [])].map((el) => el.className.split(" ")[0]);
    const at = right.indexOf("topbar-model");
    expect(right.slice(at, at + 3)).toEqual(["topbar-model", "topbar-effort", "topbar-mode"]);
  });

  it("draws the level in force by its display name", async () => {
    // Arrange / Act
    await withTopbar({ effort: "medium" });
    // Assert
    expect(harness.text(".topbar-effort-button")).toBe("medium");
  });

  it("lists exactly the levels the view carries", async () => {
    // Arrange
    await withTopbar({ effort: "medium" });
    // Act
    await harness.click(".topbar-effort");
    // Assert
    expect(harness.$$("[data-effort-option]").map((el) => el.textContent)).toEqual(
      EFFORT_LEVELS.map((option) => option.displayName),
    );
  });

  it("calls SetEffort echoing the option's own level", async () => {
    // Arrange
    await withTopbar({ effort: "medium" });
    await harness.click(".topbar-effort");
    // Act
    await harness.click('[data-effort-option="HIGH"]');
    // Assert
    const [request] = harness.fake.calls<{ effort: AgentEffortLevel }>("setEffort");
    expect(request.effort).toBe(AgentEffortLevel.HIGH);
  });

  it("puts no row in the feed for a pick", async () => {
    // Arrange
    await withTopbar({ effort: "medium" });
    const before = harness.$$("[data-feed-row]").length;
    await harness.click(".topbar-effort");
    // Act
    await harness.click('[data-effort-option="HIGH"]');
    // Assert
    expect(harness.$$("[data-feed-row]").length).toBe(before);
  });

  it("carries the refused arm at the selector", async () => {
    // Arrange
    await withTopbar({ effort: "medium" });
    harness.fake.refuse("setEffort", "notSupported");
    await harness.click(".topbar-effort");
    // Act
    await harness.click('[data-effort-option="HIGH"]');
    // Assert
    expect(harness.$(".topbar-effort .refusal")?.dataset.arm).toBe("notSupported");
  });

  it("draws a dash with no dropdown for a model that takes no level", async () => {
    // Arrange
    await withTopbar({ effort: "unsupported" });
    // Act
    await harness.click(".topbar-effort");
    // Assert
    expect([harness.text("[data-effort-unsupported]"), harness.$('.topbar-reveal[data-reveal="effort"]')]).toEqual([
      "—",
      null,
    ]);
  });

  it("draws the no-session dash when the view carries no selector", async () => {
    // Arrange / Act
    await withTopbar({});
    // Assert
    expect(harness.$('[data-no-session="effort"]')).not.toBeNull();
  });

  it("carries the owner's hover copy on both selectors", async () => {
    // Arrange / Act
    await withTopbar({ effort: "medium" });
    // Assert
    expect([harness.$(".topbar-model")?.title, harness.$(".topbar-effort")?.title]).toEqual([
      MODEL_TOOLTIP,
      EFFORT_TOOLTIP,
    ]);
  });
});

describe("the permission-mode picker", () => {
  it("draws the current mode's display name", async () => {
    // Arrange / Act
    await withTopbar({ permissionMode: "acceptEdits" });
    // Assert
    expect(harness.text(".topbar-mode")).toContain("accept edits");
  });

  it("lists exactly the options the view carries", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-mode");
    // Assert
    expect(harness.$$("[data-mode-option]").map((el) => el.dataset.modeOption)).toEqual(
      PERMISSION_MODES.map((m) => m.mode),
    );
  });

  it("offers no default option", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-mode");
    // Assert
    expect(harness.$$("[data-mode-option]").map((el) => el.dataset.modeOption)).not.toContain(
      "default",
    );
  });

  it("lists auto first", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-mode");
    // Assert
    expect(harness.$$("[data-mode-option]")[0]?.dataset.modeOption).toBe("auto");
  });

  it("draws a live default as the current mode without offering it", async () => {
    // Arrange: a session started before the 2026-09-14 auto ruling.
    await withTopbar({ permissionMode: LIVE_DEFAULT_MODE.mode });
    // Act
    await harness.click(".topbar-mode");
    // Assert
    expect(harness.text(".topbar-mode-button")).toBe(LIVE_DEFAULT_MODE.displayName);
  });

  it("draws each option's display name verbatim", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-mode");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="mode"]')).toContain("accept edits");
  });

  it("calls SetPermissionMode echoing the option's own mode string", async () => {
    // Arrange
    await withTopbar({});
    await harness.click(".topbar-mode");
    // Act
    await harness.click('[data-mode-option="plan"]');
    // Assert
    const [request] = harness.fake.calls<{ mode: string }>("setPermissionMode");
    expect(request.mode).toBe("plan");
  });

  it("draws the refusal at the mode button", async () => {
    // Arrange: a TYPED cause (see the model picker's note above).
    await withTopbar({});
    harness.fake.refuse("setPermissionMode", "modeNotServed");
    await harness.click(".topbar-mode");
    // Act
    await harness.click('[data-mode-option="plan"]');
    // Assert
    expect(harness.$(".topbar-mode .refusal")).not.toBeNull();
  });

  it("carries the refused arm at the mode button", async () => {
    // Arrange
    await withTopbar({});
    harness.fake.refuse("setPermissionMode", "ungatedWithoutConsent");
    await harness.click(".topbar-mode");
    // Act
    await harness.click('[data-mode-option="plan"]');
    // Assert
    expect(harness.$(".topbar-mode .refusal")?.dataset.arm).toBe("ungatedWithoutConsent");
  });

  it("reports a malformed view for an error with no cause", async () => {
    // Arrange
    await withTopbar({});
    harness.fake.answer(
      "setPermissionMode",
      create(SetPermissionModeResponseSchema, { result: { case: "error", value: {} } }),
    );
    await harness.click(".topbar-mode");
    // Act
    await harness.click('[data-mode-option="plan"]');
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });
});

describe("the context chip", () => {
  it("draws the served figure verbatim", async () => {
    // Arrange / Act
    await withTopbar({ contextText: "184k" });
    // Assert
    expect(harness.text(".topbar-context")).toContain("184k");
  });

  it("paints the figure by its window fill with the REAL stylesheet over the whole strip", async () => {
    // The drawing code decides a color; this is what a reader gets, and the
    // two once disagreed: a later same-specificity topbar-button rule painted
    // the one colored number in the strip GREY in the running application
    // while every class assertion stayed green. This keeps that caught for the
    // fill's color, which is the footer percentages' own rule.
    // Arrange
    const remove = installStylesheet();
    const probe = document.createElement("span");
    probe.style.color = pressurePercentColor(72);
    // Act
    await withTopbar({ contextText: "184k", windowFill: 0.72 });
    // Assert
    expect(cascadedValue(harness.$(".topbar-context-figure") as Element, "color")).toBe(
      probe.style.color,
    );
    remove();
  });

  it("reveals the breakdown's section heading verbatim", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-context");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="context"]')).toContain("session");
  });

  it("reveals every breakdown row's label verbatim", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-context");
    // Assert
    const drawn = harness.text('.topbar-reveal[data-reveal="context"]') ?? "";
    expect(drawn).toContain("system prompt");
    expect(drawn).toContain("memory files");
  });

  it("draws the share when the row carries one", async () => {
    // Arrange
    await withTopbar({ shares: true });
    // Act
    await harness.click(".topbar-context");
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="context"] [data-share]')).not.toBeNull();
  });

  it("omits the share when the row carries none", async () => {
    // Arrange
    await withTopbar({ shares: false });
    // Act
    await harness.click(".topbar-context");
    // Assert: share_permille is `optional` — absent means draw nothing.
    expect(harness.$('.topbar-reveal[data-reveal="context"] [data-share]')).toBeNull();
  });

  it("marks the emphasized row", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-context");
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="context"] [data-emphasized="true"]')).not.toBeNull();
  });

  it("indents a row by its served depth", async () => {
    // Arrange
    await withTopbar({});
    // Act
    await harness.click(".topbar-context");
    // Assert
    expect(
      harness.$('.topbar-reveal[data-reveal="context"] [data-depth="1"]'),
    ).not.toBeNull();
  });
});

describe("the warning strip", () => {
  it("draws no strip for an empty warning list", async () => {
    // Arrange / Act
    await withTopbar({ warnings: [] });
    // Assert
    expect(harness.$(".topbar-warnings")).toBeNull();
  });

  it("draws the strip once a warning arrives", async () => {
    // Arrange / Act
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    // Assert
    expect(harness.$(".topbar-warnings")).not.toBeNull();
  });

  it.each(TOPBAR_WARNING_ARMS)("draws the %s warning's line verbatim", async (arm) => {
    // Arrange / Act
    await withTopbar({ warnings: [topbarWarning(arm)] });
    // Act
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warnings"]')).toContain(`warning: ${arm}`);
  });

  it.each(TOPBAR_WARNING_ARMS)("carries the %s arm on its list row", async (arm) => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning(arm)] });
    // Act
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.$(`.topbar-warning-row[data-arm="${arm}"]`)).not.toBeNull();
  });

  it("draws every warning in the list", async () => {
    // Arrange
    await withTopbar({ warnings: TOPBAR_WARNING_ARMS.map((arm) => topbarWarning(arm)) });
    // Act
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.$$(".topbar-warning-row")).toHaveLength(TOPBAR_WARNING_ARMS.length);
  });

  it("draws the reveal below the strip", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    // Act
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="warnings"]')).not.toBeNull();
  });
});

/** What each warning's DETAIL overlay must show once its row is opened. */
const WARNING_DETAILS = [
  { arm: "accounting" as const, expected: "cache reads uncounted" },
  { arm: "unmodeledTool" as const, expected: "mcp__weather__forecast" },
  { arm: "detachedUnmodeled" as const, expected: "mcp__weather__watch" },
  { arm: "sessionFault" as const, expected: "store writes rejected" },
  { arm: "degradedWindow" as const, expected: "disk pressure" },
  { arm: "deployFailed" as const, expected: "rename store: permission denied" },
];

describe.each(WARNING_DETAILS)("the $arm warning's detail", ({ arm, expected }) => {
  it("opens on the list row", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning(arm)] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click(`.topbar-warning-row[data-arm="${arm}"]`);
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="warning-detail"]')).not.toBeNull();
  });

  it("draws its own evidence verbatim", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning(arm)] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click(`.topbar-warning-row[data-arm="${arm}"]`);
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).toContain(expected);
  });
});

describe("the session-fault detail", () => {
  it("draws the faulted component verbatim beside the detail", async () => {
    // Arrange: the daemon projects a session fault into the PUSHED view; the
    // strip is its only home, and the component names who faulted.
    await withTopbar({ warnings: [topbarWarning("sessionFault")] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click('.topbar-warning-row[data-arm="sessionFault"]');
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).toContain("shim");
  });
});

/**
 * A FAILED DEPLOY (owner request, 2026-09-28): the daemon raises it on every
 * workspace's topbar; its row opens what failed, and the page logs it loudly
 * the first time it appears.
 */
describe("the failed-deploy detail", () => {
  it("draws what became of the install beside the step", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("deployFailed")] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click('.topbar-warning-row[data-arm="deployFailed"]');
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"] [data-datum="rollback"]')).toBe(
      "it was rolled back to the previous build",
    );
  });

  it("is logged to the daemon at ERROR when it first appears on the page", async () => {
    // Arrange / Act: the page boots over a standing failed deploy.
    harness = await startHarness({
      clientLog: true,
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [topbarWarning("deployFailed")] })),
    });
    await harness.settle();
    // Assert
    const records = harness.fake
      .calls<{ record?: { operation: string; level: { case: string } } }>("clientLog")
      .map((call) => call.record)
      .filter((record) => record?.operation === "topbar.deploy-failed");
    expect(records.map((record) => record?.level.case)).toEqual(["error"]);
  });
});

describe("the unmodeled-tool detail", () => {
  it("draws every argument line verbatim", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("unmodeledTool")] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click('.topbar-warning-row[data-arm="unmodeledTool"]');
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).toContain("days=3");
  });
});

describe("the degraded-window detail", () => {
  it("ticks while the window is open", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("degradedWindow")] });
    await harness.click(".topbar-warnings");
    await harness.click('.topbar-warning-row[data-arm="degradedWindow"]');
    const before = harness.text('.topbar-reveal[data-reveal="warning-detail"]');
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).not.toBe(before);
  });

  it("draws a settled extent rather than ticking once closed", async () => {
    // Arrange
    await withTopbar({ warnings: [degradedWindowClosedWarning()] });
    await harness.click(".topbar-warnings");
    await harness.click('.topbar-warning-row[data-arm="degradedWindow"]');
    const before = harness.text('.topbar-reveal[data-reveal="warning-detail"]');
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).toBe(before);
  });

  it("draws the closed window's dropped count verbatim", async () => {
    // Arrange
    await withTopbar({ warnings: [degradedWindowClosedWarning()] });
    await harness.click(".topbar-warnings");
    // Act
    await harness.click('.topbar-warning-row[data-arm="degradedWindow"]');
    // Assert
    expect(harness.text('.topbar-reveal[data-reveal="warning-detail"]')).toContain("12");
  });
});

describe("the login terminal", () => {
  /**
   * Boot logged out and open the overlay THE WAY A READER NOW DOES.
   *
   * THE CELL'S CLICK IS THE LOGIN OPTIONS, not OpenLogin (owner ruling,
   * 2026-09-13, `src/topbar/account.ts`): the cell opens the dropdown of every
   * root the daemon knows, and PICKING one is the rpc. A root with no login is
   * still a successful pick — `SelectAccountSuccess.logged_in` false is the
   * client's cue to open that root's login flow behind it, which is what puts
   * the terminal on screen. These scenarios assert the login pty end to end,
   * so they take the one route that reaches it.
   */
  const openLogin = async (): Promise<void> => {
    await withTopbar({ account: "loggedOut" });
    await harness.click(".topbar-account");
    await harness.click(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`);
  };

  it("calls OpenLogin when a root with no login is picked", async () => {
    // Arrange / Act
    await openLogin();
    // Assert
    expect(harness.fake.calls("openLogin")).toHaveLength(1);
  });

  it("calls no OpenLogin for the cell's own click", async () => {
    // Arrange: the cell opens the CHOICE; the pick is what calls anything.
    await withTopbar({ account: "loggedOut" });
    // Act
    await harness.click(".topbar-account");
    // Assert
    expect(harness.fake.calls("openLogin")).toHaveLength(0);
  });

  it("opens the options dropdown from the cell's own click", async () => {
    // Arrange
    await withTopbar({ account: "loggedOut" });
    // Act
    await harness.click(".topbar-account");
    // Assert
    expect(harness.$(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`)).not.toBeNull();
  });

  it("opens the overlay", async () => {
    // Arrange / Act
    await openLogin();
    // Assert
    expect(harness.$('[data-component="login-overlay"]')?.hidden).toBe(false);
  });

  it("does not open the overlay for a root that holds a login", async () => {
    // Arrange: a picked root that IS logged in needs no login flow.
    await withTopbar({ account: "loggedIn" });
    await harness.click(".topbar-account");
    // Act
    await harness.click(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`);
    // Assert
    expect(harness.fake.calls("openLogin")).toHaveLength(0);
  });

  it("echoes the picked root's config dir verbatim", async () => {
    // Arrange
    await withTopbar({
      account: "loggedIn",
      accountOptions: [
        { configDir: ACCOUNT_CONFIG_DIR, email: "dev@example.test", current: true },
        { configDir: OTHER_ACCOUNT_CONFIG_DIR },
      ],
    });
    await harness.click(".topbar-account");
    // Act
    await harness.click(`[data-account-option="${OTHER_ACCOUNT_CONFIG_DIR}"]`);
    // Assert
    const [request] = harness.fake.calls<{ configDir: string }>("selectAccount");
    expect(request.configDir).toBe(OTHER_ACCOUNT_CONFIG_DIR);
  });

  it("opens the login flow for a picked root that holds none", async () => {
    // Arrange
    await withTopbar({
      account: "loggedIn",
      accountOptions: [
        { configDir: ACCOUNT_CONFIG_DIR, email: "dev@example.test", current: true },
        { configDir: OTHER_ACCOUNT_CONFIG_DIR },
      ],
    });
    await harness.click(".topbar-account");
    // Act
    await harness.click(`[data-account-option="${OTHER_ACCOUNT_CONFIG_DIR}"]`);
    // Assert
    expect(harness.fake.calls("openLogin")).toHaveLength(1);
  });

  it("attaches the terminal stream for the workspace", async () => {
    // Arrange / Act
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("watchLoginTerminal");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("draws the replayed scrollback into the terminal", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => {
        fake.setTopbar(WORKSPACE_ID, topbarView({ account: "loggedOut" }));
        fake.setLoginScrollback(WORKSPACE_ID, [new TextEncoder().encode("welcome back")]);
      },
    });
    // Act
    await harness.click(".topbar-account");
    await harness.click(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`);
    await harness.fake.awaitStream("watchLoginTerminal");
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="login-overlay"]')?.textContent).toContain("welcome back");
  });

  it("draws a live byte push into the terminal", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    harness.fake.pushLoginBytes(WORKSPACE_ID, new TextEncoder().encode("enter your code"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="login-overlay"]')?.textContent).toContain("enter your code");
  });

  it("sends a keystroke as SendLoginInput's keystrokes arm", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    harness
      .$('[data-component="login-overlay"] [data-login-term]')
      ?.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    await harness.settle();
    // Assert
    const requests = harness.fake.calls<{ input: { case?: string } }>("sendLoginInput");
    expect(requests.some((r) => r.input.case === "keystrokes")).toBe(true);
  });

  it("sends a resize as SendLoginInput's resize arm", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    window.dispatchEvent(new Event("resize"));
    await harness.settle();
    // Assert
    const requests = harness.fake.calls<{ input: { case?: string } }>("sendLoginInput");
    expect(requests.some((r) => r.input.case === "resize")).toBe(true);
  });

  it("echoes the workspace on every login input", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    window.dispatchEvent(new Event("resize"));
    await harness.settle();
    // Assert
    const requests = harness.fake.calls<{ workspace?: { id: string } }>("sendLoginInput");
    expect(requests.every((r) => r.workspace?.id === WORKSPACE_ID)).toBe(true);
  });

  it("closes the overlay on the terminal's closed frame", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    harness.fake.closeLoginTerminal(WORKSPACE_ID);
    await harness.settle();
    // Assert
    expect(harness.$('[data-component="login-overlay"]')?.hidden).toBe(true);
  });

  it("reports no transport failure for a closed frame", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act: `closed` concludes the stream legitimately.
    harness.fake.closeLoginTerminal(WORKSPACE_ID);
    await harness.tick(5_000);
    // Assert
    expect(harness.failureArms()).not.toContain("daemonUnreachable");
  });

  it("calls CloseLogin when the close button is clicked", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    await harness.click("[data-login-close]");
    // Assert
    expect(harness.fake.calls("closeLogin")).toHaveLength(1);
  });

  it("hides the overlay when the close button is clicked", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    await harness.click("[data-login-close]");
    // Assert
    expect(harness.$('[data-component="login-overlay"]')?.hidden).toBe(true);
  });

  it("cancels the terminal stream when the overlay closes", async () => {
    // Arrange
    await openLogin();
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act
    await harness.click("[data-login-close]");
    await harness.settle();
    // Assert
    expect(harness.fake.liveStreams("watchLoginTerminal", WORKSPACE_ID)).toBe(0);
  });
});

// ---------------------------------------------------------------------------
// THE WARNING STRIP IS PART OF A WHOLE-VIEW PUSH (audit 1, item 11)
//
// "Whole-view pushes replace their unit whole; nothing accumulates across
// pushes." A `TopbarView` with an empty warning list is the daemon saying
// there are no warnings — so the strip goes, rather than standing on the
// strength of an older push.
// ---------------------------------------------------------------------------

describe("the warning strip's omission", () => {
  it("removes the strip when the next push carries no warnings", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    // Act
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [] }));
    await harness.settle();
    // Assert
    expect(harness.$(".topbar-warnings")).toBeNull();
  });

  it("drops a warning the next push omits", async () => {
    // Arrange
    await withTopbar({
      warnings: [topbarWarning("accounting"), topbarWarning("unmodeledTool")],
    });
    // Act
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [topbarWarning("accounting")] }));
    await harness.settle();
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.$$(".topbar-warning-row").map((el) => el.dataset.arm)).toEqual(["accounting"]);
  });

  it("closes the reveal along with the strip it belonged to", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    await harness.click(".topbar-warnings");
    // Act
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [] }));
    await harness.settle();
    // Assert
    expect(harness.$('.topbar-reveal[data-reveal="warnings"]')).toBeNull();
  });

  it("draws the strip again when a warning comes back", async () => {
    // Arrange
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [] }));
    await harness.settle();
    // Act
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [topbarWarning("sessionFault")] }));
    await harness.settle();
    await harness.click(".topbar-warnings");
    // Assert
    expect(harness.$('.topbar-warning-row[data-arm="sessionFault"]')).not.toBeNull();
  });

  it("reports no failure for an empty warning list", async () => {
    // Arrange / Act: an empty repeated field is a fact, not a malformed view.
    await withTopbar({ warnings: [topbarWarning("accounting")] });
    harness.fake.setTopbar(WORKSPACE_ID, topbarView({ warnings: [] }));
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// THE LOGIN PTY DYING WITHOUT `closed` (audit 1, item 13)
//
// `closed` is the pty's legitimate end — the login child exited. A stream that
// ends ANY other way is a transport failure and is reported, because a terminal
// that has silently stopped taking keystrokes is indistinguishable from one
// waiting for the user's next character.
// ---------------------------------------------------------------------------

describe("the login terminal's transport death", () => {
  /** Boot logged out, open the overlay, and kill the pty stream mid-flight. */
  const killTerminal = async (): Promise<void> => {
    // The cell opens the options; picking the logged-out root is what opens
    // the login flow (owner ruling, 2026-09-13 — see the helper above).
    await withTopbar({ account: "loggedOut" });
    await harness.click(".topbar-account");
    await harness.click(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`);
    await harness.fake.awaitStream("watchLoginTerminal");
    harness.fake.endStream("watchLoginTerminal", WORKSPACE_ID);
    await harness.tick(1_000);
  };

  it("reports the failure", async () => {
    // Arrange / Act
    await killTerminal();
    // Assert
    expect(harness.failureArms()).toContain("controlPlaneFailed");
  });

  it("names the login terminal in the report", async () => {
    // Arrange / Act
    await killTerminal();
    // Assert
    expect(await chipFailureText(harness, "controlPlaneFailed")).toContain("login terminal");
  });

  it("does not re-probe the pty", async () => {
    // Arrange / Act: nothing is re-attached; the login child is gone.
    await killTerminal();
    const attempts = harness.fake.calls("watchLoginTerminal").length;
    await harness.tick(10_000);
    // Assert
    expect(harness.fake.calls("watchLoginTerminal").length).toBe(attempts);
  });

  it("reports nothing for the pty's own closed frame", async () => {
    // Arrange
    await withTopbar({ account: "loggedOut" });
    await harness.click(".topbar-account");
    await harness.click(`[data-account-option="${ACCOUNT_CONFIG_DIR}"]`);
    await harness.fake.awaitStream("watchLoginTerminal");
    // Act: `closed` is the legitimate end.
    harness.fake.closeLoginTerminal(WORKSPACE_ID);
    await harness.tick(1_000);
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });
});

/**
 * PRESENCE, NEVER A SENTINEL. `TopbarModelSelector.selected` is `optional`, so
 * an unset selection is a state the daemon can legitimately serve — the chip
 * says so with its placeholder rather than naming the first option as though
 * it had been picked.
 */
describe("the model selector with nothing selected", () => {
  it("draws the placeholder", async () => {
    // Arrange / Act
    await withTopbar({ unselected: true });
    // Assert
    expect(harness.text(".topbar-model")).toBe(MODEL_PLACEHOLDER);
  });

  it("marks the chip as unselected", async () => {
    // Arrange / Act
    await withTopbar({ unselected: true });
    // Assert
    expect(harness.$(".topbar-model-button")?.hasAttribute("data-unselected")).toBe(true);
  });

  it("names no model at all", async () => {
    // Arrange / Act
    await withTopbar({ unselected: true });
    // Assert: absent, not empty — an empty name would read as a nameless model.
    expect(harness.$(".topbar-model")?.hasAttribute("data-model")).toBe(false);
  });

  it("still lists every option the view carries", async () => {
    // Arrange
    await withTopbar({ unselected: true });
    // Act
    await harness.click(".topbar-model");
    // Assert
    expect(harness.$$("[data-model-option]").map((el) => el.dataset.modelOption)).toEqual([
      "opus",
      "sonnet",
    ]);
  });

  it("does not mark a selected chip as unselected", async () => {
    // Arrange / Act
    await withTopbar({});
    // Assert
    expect(harness.$(".topbar-model-button")?.hasAttribute("data-unselected")).toBe(false);
  });
});

describe("the detached-unmodeled detail", () => {
  const openDetail = async (): Promise<void> => {
    await withTopbar({ warnings: [topbarWarning("detachedUnmodeled")] });
    await harness.click(".topbar-warnings");
    await harness.click('.topbar-warning-row[data-arm="detachedUnmodeled"]');
  };

  it("names the tool that is still running", async () => {
    // Arrange / Act
    await openDetail();
    // Assert
    expect(harness.text('[data-reveal="warning-detail"] .topbar-warning-body')).toContain(
      "mcp__weather__watch",
    );
  });

  it("reads its age from the served start instant", async () => {
    // Arrange / Act: started 1 s, the page's clock starts at the epoch (10 s).
    await openDetail();
    // Assert
    expect(harness.text(".topbar-warning-clock")).toBe("running 9s");
  });

  it("ticks the age while the detail stands open", async () => {
    // Arrange
    await openDetail();
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.text(".topbar-warning-clock")).toBe("running 14s");
  });
});

/**
 * THE STRIP'S SPACING, ON THE WHOLE MOUNTED PAGE.
 *
 * The owner's ruling, 2026-09-14, verbatim: "the whitespace/padding on the
 * left and right of the center text should be equal, which is subtly
 * different than it being centered. Currently it is centered in the topbar's
 * full span, but it'd be better if it was centered in the space between the
 * login section (left) and the chips section (right)." It supersedes the
 * "true center" rule of 2026-09-13 and the measured cap that served it.
 *
 * The flanks are `auto` — each sized to its own group — and the middle track
 * is `minmax(0, 1fr)`, so the title takes exactly what is between them and
 * `text-align: center` centers the text in that. jsdom lays nothing out, so
 * what is assertable here is the rule the cascade hands the mounted row.
 */
describe("the strip's spacing", () => {
  /** The row as the mounted page has it, with the real stylesheet installed. */
  async function mountedRow(): Promise<Element> {
    // A WIDE RIGHT GROUP AND A NARROW LEFT ONE: the asymmetry the owner saw.
    await withTopbar({
      title: "port the webapp",
      email: "d@e.io",
      models: [
        {
          name: "fake-opus-4-8",
          displayName: "Opus 4.8 (the long one)",
          description: "the big one",
        },
      ],
      contextText: "184.3k",
      warnings: [topbarWarning("sessionFault"), topbarWarning("detachedUnmodeled")],
    });
    const row = harness.$(".topbar-row");
    if (!row) throw new Error("the topbar drew no row");
    return row;
  }

  /** The three tracks of the row's `grid-template-columns`, in order. */
  function tracks(template: string): string[] {
    const parts: string[] = [];
    let depth = 0;
    let current = "";
    for (const ch of template) {
      if (ch === "(") depth += 1;
      if (ch === ")") depth -= 1;
      if (ch === " " && depth === 0) {
        if (current !== "") parts.push(current);
        current = "";
        continue;
      }
      current += ch;
    }
    if (current !== "") parts.push(current);
    return parts;
  }

  it("sizes each flank track to its own group", async () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const row = await mountedRow();
      // Act
      const declared = tracks(cascadedValue(row, "grid-template-columns"));
      // Assert: `auto`, so neither flank spreads past what it holds.
      expect([declared[0], declared[2]]).toEqual(["auto", "auto"]);
    } finally {
      teardown();
    }
  });

  it("gives the title the space between the two groups and no more", async () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const row = await mountedRow();
      // Act
      const declared = tracks(cascadedValue(row, "grid-template-columns"));
      // Assert
      expect(declared[1]).toBe("minmax(0, 1fr)");
    } finally {
      teardown();
    }
  });

  it("centers the title's text inside that middle track", async () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const row = await mountedRow();
      const title = row.querySelector(".topbar-title");
      if (!title) throw new Error("the topbar drew no title");
      // Act / Assert: equal clear space either side is what this buys.
      expect(cascadedValue(title, "text-align")).toBe("center");
    } finally {
      teardown();
    }
  });

  it("draws the title between the two flank groups", async () => {
    // Arrange / Act
    const row = await mountedRow();
    // Assert: the middle track holds the title and nothing else does.
    expect([...row.children].map((el) => el.className.split(" ")[0])).toEqual([
      "topbar-left",
      "topbar-title",
      "topbar-right",
    ]);
  });

  it("lets the wide right group compress into its track", async () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const row = await mountedRow();
      const right = row.querySelector(".topbar-right");
      if (!right) throw new Error("the topbar drew no right group");
      // Act / Assert: without this the group spills out of its floorless
      // track and over the title.
      expect(cascadedValue(right, "min-width")).toBe("0px");
    } finally {
      teardown();
    }
  });
});
