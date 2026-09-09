/**
 * §F6 — COMMAND PANELS, against the real chain.
 *
 * A slash command the daemon recognizes is answered PROGRAMMATICALLY: the
 * daemon resolves the panel's rows and returns them as a `SubmitPrompt`
 * SUCCESS ARM, never over a stream, and the webapp owns only the rendering.
 * The panel is drawn in the composer's area, and NO feed row is minted for it.
 *
 * NO FAKE-SDK SCENARIO IS INVOLVED, and none is invented: this path never
 * reaches the vendor at all (the daemon's recognition table intercepts the
 * command). The Go slash-command area covers the opposite case — a command the
 * VENDOR answers — and says so in its own header, so this area has no Go
 * scenario to reuse and needs none.
 *
 * The commands submitted below are literals from the contract's own
 * recognition table (`conversation/v1/slash_command.proto`'s
 * `session_command_spec`), not names this file chose.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { BOOT_BUDGET_MS, TURN_TEST_MS, awaitDrawn, bootLayer, rows, submit } from "./drive";

let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/** Every panel currently drawn in the composer's area. */
function panels(): HTMLElement[] {
  return app.$$('[data-component="composer"] [data-panel]');
}

/**
 * Submit a recognized slash command and wait for ITS OWN panel to be drawn.
 *
 * THE WAIT IS ON A NEW NODE, NOT ON A NON-EMPTY HOST, and the difference is a
 * whole test's verdict. Every command in this file leaves its panel standing,
 * so from the second one onward `panels().length > 0` is ALREADY TRUE when the
 * submission is made: `command` returned before this answer had been applied
 * at all, and "clears the composer's text when the command is answered" then
 * read the box while the daemon was still answering and found the literal it
 * had just typed. Once in eight in-container runs.
 *
 * `main.ts`'s `showPanel` removes the stale panels and APPENDS a fresh
 * element, so a node that is not the one standing before the submit is this
 * submission's answer and nothing else. Order-independent: the first command
 * in a file has no standing panel, and `undefined !== node` holds for it too.
 */
async function command(literal: string): Promise<HTMLElement> {
  const standing = panels().at(-1);
  await submit(app, literal);
  await awaitDrawn(
    app,
    `the panel answering ${literal}`,
    () => panels().length > 0 && panels().at(-1) !== standing,
  );
  const drawn = panels();
  return drawn[drawn.length - 1];
}

/**
 * A LIVE SESSION FIRST.
 *
 * Every panel here reports on a SESSION — its status, its context budget, the
 * commands available in it — and a workspace that has never run a prompt has
 * no session for the daemon to report on: `/status` comes back as a transport
 * refusal and `/help` as a refused command. So the file opens one real turn
 * (the fake SDK's default PROSE turn, no scenario prefix) before any command,
 * exactly as a user would have by the time they typed one.
 */
beforeAll(async () => {
  const before = rows(app, "turnEnded").length;
  await submit(app, "start a session for the panel commands");
  await awaitDrawn(app, "the opening turn to end", () => rows(app, "turnEnded").length > before);
}, TURN_TEST_MS);

// §F6 #28.
it(
  "draws the panel answering a recognized slash command, and mints no feed row for it",
  async () => {
    // Arrange — count the rows standing before the command.
    const before = rows(app, "userPrompt").length;
    const terminalsBefore = rows(app, "turnEnded").length;

    // Act — a command the daemon answers itself.
    const panel = await command("/status");

    // Assert — the panel is drawn in the composer's own area, and the command
    // produced no conversation at all: no prompt row, no turn.
    expect(panel).not.toBeNull();
    expect(rows(app, "userPrompt").length).toBe(before);
    expect(rows(app, "turnEnded").length).toBe(terminalsBefore);
  },
  TURN_TEST_MS,
);

it(
  "clears the composer's text when the command is answered",
  async () => {
    // Arrange / Act
    await command("/status");

    // Assert — a successful submission empties the box; the answer is not a
    // refusal, so nothing is kept for a retry.
    const input = app.$('[data-component="composer"] textarea') as HTMLTextAreaElement | null;
    expect(input?.value).toBe("");
  },
  TURN_TEST_MS,
);

it(
  "draws no refusal for a command the daemon answered",
  async () => {
    // Arrange / Act
    await command("/status");

    // Assert — a panel is a SUCCESS arm.
    expect(app.refusalArms()).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F6 #29 — a panel answers ONE submission, not the conversation.
it(
  "replaces the standing panel when a second command is answered",
  async () => {
    // Arrange — one panel standing.
    const first = await command("/status");
    expect(panels()).toHaveLength(1);

    // Act — a different command.
    const second = await command("/context");

    // Assert — still exactly one panel, and it is the new one.
    expect(panels()).toHaveLength(1);
    expect(second).not.toBe(first);
    expect(first.isConnected).toBe(false);
  },
  TURN_TEST_MS,
);

it(
  "draws the help panel's own commands",
  async () => {
    // Arrange / Act
    const panel = await command("/help");

    // Assert — the panel carries rows resolved by the daemon; the client only
    // renders them.
    expect(panel.textContent?.trim()).not.toBe("");
  },
  TURN_TEST_MS,
);
