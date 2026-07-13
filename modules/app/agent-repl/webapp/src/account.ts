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
