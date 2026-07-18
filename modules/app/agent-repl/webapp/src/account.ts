/**
 * Which Claude account this session runs as.
 *
 * The topbar names it beside the connection status. With two accounts in
 * play (a personal ~/.claude and a work ~/.claude-chesscom, selected by the
 * workspace's path), "which account am I about to spend tokens as" is not
 * something a user should have to infer — and the wrong one is both easy to
 * reach and expensive to notice.
 *
 * The daemon does the resolving: it reads the email out of the .claude.json
 * belonging to the session's own CLAUDE_CONFIG_DIR.
 */

/** Text shown when the account's config root has no login. */
export const ACCOUNT_LOGGED_OUT = "logged out";

/** Who a session's config root is logged in as. */
export interface Account {
  /** CLAUDE_CONFIG_DIR, empty for the CLI's own default root. */
  config_dir: string;
  /** The logged-in email, empty when the root has no login. */
  email: string;
}

/**
 * Fetch the account this session runs as.
 *
 * Rejects on any non-2xx. A logged-OUT account is not an error — it comes
 * back as a 200 with an empty email, because that is a state to render.
 */
export async function fetchAccount(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<Account> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/account`);
  if (!resp.ok) {
    throw new Error(
      `GET /sessions/${sessionId}/account: ${resp.status} ${await resp.text()}`,
    );
  }
  return (await resp.json()) as Account;
}

/**
 * Topbar text for an account.
 *
 * A logged-out root says so rather than rendering blank: an empty slot reads
 * as "still loading", which is the one thing it is not.
 */
export function accountLabel(account: Account | null): string {
  if (account === null) return "";
  return account.email === "" ? ACCOUNT_LOGGED_OUT : account.email;
}

/**
 * Whether the topbar should render the account as a problem.
 *
 * A session whose config root is logged out cannot run a turn, so the label
 * is the warning, not a decoration.
 */
export function accountIsLoggedOut(account: Account | null): boolean {
  return account !== null && account.email === "";
}

/** One canonical root of the daemon's -accounts roster, with its identity. */
export interface RosterEntry {
  /** The operator's name for the root ("personal", "work"). */
  label: string;
  /** CLAUDE_CONFIG_DIR selecting the root, empty for the CLI default. */
  config_dir: string;
  /** The root's logged-in email, empty when logged out. */
  email: string;
  /** A per-root identity read failure (corrupt .claude.json), if any. */
  error?: string;
}

/**
 * Fetch the canonical account roster.
 *
 * Rejects on any non-2xx, including the 503 of a daemon started without
 * -accounts — the menu then degrades to its re-auth entry alone.
 */
export async function fetchAccounts(
  httpBase: string,
  fetchFn: typeof fetch = fetch,
): Promise<RosterEntry[]> {
  const resp = await fetchFn(`${httpBase}/accounts`);
  if (!resp.ok) {
    throw new Error(`GET /accounts: ${resp.status} ${await resp.text()}`);
  }
  const body = (await resp.json()) as { accounts: RosterEntry[] };
  return body.accounts;
}

/** What a switch request came back with. */
export interface SwitchOutcome {
  /** False when the session already ran as the target root. */
  switched: boolean;
  /** The target root, with the identity the session now runs as. */
  account: RosterEntry;
}

/**
 * Move the session onto another canonical root.
 *
 * The daemon migrates the transcript and bounces the shim under the new
 * CLAUDE_CONFIG_DIR; the session id survives, so the caller's stream
 * reconnect finds the same conversation. Rejects on any non-2xx — a
 * mid-turn 409 and an off-roster 400 both surface to the topbar.
 */
export async function switchAccount(
  httpBase: string,
  sessionId: string,
  configDir: string,
  fetchFn: typeof fetch = fetch,
): Promise<SwitchOutcome> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/account`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ config_dir: configDir }),
  });
  if (!resp.ok) {
    throw new Error(
      `POST /sessions/${sessionId}/account: ${resp.status} ${await resp.text()}`,
    );
  }
  return (await resp.json()) as SwitchOutcome;
}

/** One entry of the account chip's dropdown. */
export type AccountMenuEntry =
  | { kind: "reauth"; text: string }
  | { kind: "switch"; text: string; configDir: string };

/**
 * Derive the account menu: one re-auth verb for the root the session runs
 * as, then one switch verb per OTHER canonical root.
 *
 * The two verbs stay independent underneath — re-auth opens the login
 * terminal (OAuth, for expired creds), switch never touches OAuth (it
 * selects a root that is already authed). A root the daemon could not read
 * is offered with its problem named rather than hidden: switching to it and
 * hitting the error beats not knowing it exists.
 */
export function accountMenuEntries(
  current: Account | null,
  roster: RosterEntry[],
): AccountMenuEntry[] {
  const entries: AccountMenuEntry[] = [
    {
      kind: "reauth",
      text:
        current === null || accountIsLoggedOut(current)
          ? "log in"
          : `re-auth ${current.email}`,
    },
  ];
  for (const root of roster) {
    if (current !== null && root.config_dir === current.config_dir) continue;
    const who =
      root.error !== undefined && root.error !== ""
        ? "unreadable"
        : root.email === ""
          ? ACCOUNT_LOGGED_OUT
          : root.email;
    entries.push({
      kind: "switch",
      text: `switch to ${root.label} (${who})`,
      configDir: root.config_dir,
    });
  }
  return entries;
}

/**
 * Build the account menu from a LIVE read of BOTH the current account and
 * the roster.
 *
 * The menu filters the roster against WHICH root the session runs as, so the
 * "current" it filters against must be as fresh as the roster it filters —
 * else a switch the caller's cached account has not caught up to re-offers
 * the very root the session just moved to (personal->work leaves "switch to
 * work" showing instead of "switch to personal"). Reading both together at
 * open time is what keeps the two sides in step. Rejects when either fetch
 * fails, so the caller renders its own fallback rather than a menu built on
 * one stale half.
 */
export async function fetchAccountMenuEntries(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<{ current: Account; entries: AccountMenuEntry[] }> {
  const [current, roster] = await Promise.all([
    fetchAccount(httpBase, sessionId, fetchFn),
    fetchAccounts(httpBase, fetchFn),
  ]);
  return { current, entries: accountMenuEntries(current, roster) };
}
