// @vitest-environment jsdom
import { createControl } from "../../src/control.js";
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SelectAccountResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_account_pb";
import {
  TopbarAccountSchema,
  type TopbarAccount,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  ACCOUNT_OPTION_ATTRIBUTE,
  CURRENT_OPTION_ATTRIBUTE,
  LOGGED_OUT_LABEL,
  bindAccountReveal,
  drawAccountOptions,
} from "../../src/topbar/account.js";
import { drawTopbarAccount } from "../../src/topbar/strip.js";
import { appContext, openPanel, topbarContext } from "./fixtures.js";

/** One option row, as the cell's `options` list takes it. */
type OptionInit = {
  configDir: string;
  current: boolean;
  state:
    | { case: "loggedIn"; value: { email: string } }
    | { case: "loggedOut"; value: Record<string, never> };
};

/** One logged-in option row. */
const loggedIn = (configDir: string, email: string, current = false): OptionInit => ({
  configDir,
  current,
  state: { case: "loggedIn", value: { email } },
});

/** One logged-out option row. */
const loggedOut = (configDir: string, current = false): OptionInit => ({
  configDir,
  current,
  state: { case: "loggedOut", value: {} },
});

/** The account cell as the daemon serves it. */
function account(options: OptionInit[]): TopbarAccount {
  return create(TopbarAccountSchema, {
    state: { case: "loggedIn", value: { email: "dev@example.com" } },
    options,
  });
}

/** Mount the cell with its reveal wired, and answer with the cell button. */
function mountCell(
  tc: ReturnType<typeof topbarContext>["tc"],
  host: HTMLElement,
  view: TopbarAccount,
): HTMLElement {
  const cell = drawTopbarAccount(view);
  bindAccountReveal(cell, view, tc);
  host.append(cell);
  return cell;
}

describe("drawAccountOptions", () => {
  it("lists every root the daemon served, in the served order", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const view = account([
      loggedIn("/Users/dev/.claude", "dev@example.com", true),
      loggedIn("/Users/dev/.claude-work", "work@example.com"),
    ]);
    // ACT
    const list = drawAccountOptions(view, tc, createControl());
    host.append(list);
    // ASSERT
    const rows = [...list.querySelectorAll(`[${ACCOUNT_OPTION_ATTRIBUTE}]`)];
    expect(rows.map((row) => row.getAttribute(ACCOUNT_OPTION_ATTRIBUTE))).toEqual([
      "/Users/dev/.claude",
      "/Users/dev/.claude-work",
    ]);
  });

  it("marks the root the session already spends as", () => {
    // ARRANGE
    const { tc } = topbarContext();
    const view = account([
      loggedIn("/Users/dev/.claude", "dev@example.com", true),
      loggedIn("/Users/dev/.claude-work", "work@example.com"),
    ]);
    // ACT
    const list = drawAccountOptions(view, tc, createControl());
    // ASSERT
    const rows = list.querySelectorAll<HTMLElement>(`[${ACCOUNT_OPTION_ATTRIBUTE}]`);
    expect(rows[0].hasAttribute(CURRENT_OPTION_ATTRIBUTE)).toBe(true);
    expect(rows[1].hasAttribute(CURRENT_OPTION_ATTRIBUTE)).toBe(false);
  });

  it("says 'logged out' on a root with no login rather than drawing a blank row", () => {
    // ARRANGE
    const { tc } = topbarContext();
    const view = account([loggedOut("/Users/dev/.claude-work")]);
    // ACT
    const list = drawAccountOptions(view, tc, createControl());
    // ASSERT
    expect(list.textContent).toContain(LOGGED_OUT_LABEL);
  });

  it("names each root by its own path, which is the echo token the pick sends", () => {
    // ARRANGE
    const { tc } = topbarContext();
    const view = account([loggedIn("/Users/dev/.claude", "dev@example.com", true)]);
    // ACT
    const list = drawAccountOptions(view, tc, createControl());
    // ASSERT
    expect(list.textContent).toContain("/Users/dev/.claude");
  });
});

describe("bindAccountReveal", () => {
  it("opens the options when the cell is clicked", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const cell = mountCell(tc, host, account([loggedIn("/Users/dev/.claude", "dev@example.com", true)]));
    // ACT
    cell.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)!.querySelectorAll(`[${ACCOUNT_OPTION_ATTRIBUTE}]`).length).toBe(1);
  });

  it("offers the single root as a one-row dropdown on a one-root machine", () => {
    // ARRANGE — the ruling: present that one option, never nothing.
    const { host, tc } = topbarContext();
    const cell = mountCell(tc, host, account([loggedIn("/Users/dev/.claude", "dev@example.com", true)]));
    // ACT
    cell.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    const rows = openPanel(host)!.querySelectorAll(`[${ACCOUNT_OPTION_ATTRIBUTE}]`);
    expect(rows.length).toBe(1);
  });

  it("closes the options when the cell is clicked again", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    const cell = mountCell(tc, host, account([loggedIn("/Users/dev/.claude", "dev@example.com", true)]));
    // ACT
    cell.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    cell.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });
});

describe("the pick", () => {
  /** Click the named row of a two-root cell and let the rpc settle. */
  async function pick(
    impl: Parameters<typeof appContext>[0],
    view: TopbarAccount,
    configDir: string,
    openLogin: (control: HTMLElement) => void = () => undefined,
  ): Promise<HTMLElement> {
    const { host, tc } = topbarContext(appContext(impl), openLogin);
    const cell = mountCell(tc, host, view);
    cell.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    openPanel(host)!
      .querySelector(`[${ACCOUNT_OPTION_ATTRIBUTE}="${configDir}"]`)!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await new Promise((resolve) => setTimeout(resolve, 0));
    return host;
  }

  const twoRoots = () =>
    account([
      loggedIn("/Users/dev/.claude", "dev@example.com", true),
      loggedIn("/Users/dev/.claude-work", "work@example.com"),
    ]);

  it("sends the chosen root back verbatim", async () => {
    // ARRANGE
    let sent = "";
    // ACT
    await pick(
      {
        selectAccount: (req) => {
          sent = req.configDir;
          return create(SelectAccountResponseSchema, {
            result: { case: "success", value: { loggedIn: true } },
          });
        },
      },
      twoRoots(),
      "/Users/dev/.claude-work",
    );
    // ASSERT
    expect(sent).toBe("/Users/dev/.claude-work");
  });

  it("closes the reveal once the daemon has answered", async () => {
    // ARRANGE / ACT
    const host = await pick(
      {
        selectAccount: () =>
          create(SelectAccountResponseSchema, {
            result: { case: "success", value: { loggedIn: true } },
          }),
      },
      twoRoots(),
      "/Users/dev/.claude-work",
    );
    // ASSERT
    expect(openPanel(host)).toBeNull();
  });

  it("opens the login flow when the chosen root holds none", async () => {
    // ARRANGE
    const openLogin = vi.fn();
    // ACT
    await pick(
      {
        selectAccount: () =>
          create(SelectAccountResponseSchema, {
            result: { case: "success", value: { loggedIn: false } },
          }),
      },
      account([
        loggedIn("/Users/dev/.claude", "dev@example.com", true),
        loggedOut("/Users/dev/.claude-work"),
      ]),
      "/Users/dev/.claude-work",
      openLogin,
    );
    // ASSERT
    expect(openLogin).toHaveBeenCalledTimes(1);
  });

  it("opens no login when the chosen root already holds one", async () => {
    // ARRANGE
    const openLogin = vi.fn();
    // ACT
    await pick(
      {
        selectAccount: () =>
          create(SelectAccountResponseSchema, {
            result: { case: "success", value: { loggedIn: true } },
          }),
      },
      twoRoots(),
      "/Users/dev/.claude-work",
      openLogin,
    );
    // ASSERT
    expect(openLogin).not.toHaveBeenCalled();
  });

  it("draws the refusal at the cell when the daemon does not know the root", async () => {
    // ARRANGE / ACT
    const host = await pick(
      {
        selectAccount: () =>
          create(SelectAccountResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownAccount", value: {} } } },
          }),
      },
      twoRoots(),
      "/Users/dev/.claude-work",
    );
    // ASSERT
    expect(host.querySelector(".refusal")!.getAttribute("data-arm")).toBe("unknownAccount");
  });
});
