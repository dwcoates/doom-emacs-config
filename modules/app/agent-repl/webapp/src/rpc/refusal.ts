/**
 * refusal — the SHARED SENTENCES for the four refusal causes every
 * per-workspace rpc carries.
 *
 * Since landing 4 every `<Rpc>Error` carries a typed `cause` oneof, and four of
 * its arms are the same four everywhere: the workspace is not in the registry,
 * the echoed dir disagrees with the registry's, the daemon released the
 * workspace to a successor, a joining daemon has not adopted it yet. Those four
 * mean the same thing whichever rpc reports them, so they are worded ONCE here
 * rather than a dozen times at the call sites — a user who sees "transferring
 * away" from a merge queue entry and from the sidebar reads the same sentence
 * about the same fact.
 *
 * PER-RPC ARMS ARE THE CALL SITE'S. This returns `undefined` for any arm it
 * does not own, which is the caller's signal to supply its own wording; it
 * never invents a sentence for an arm it does not know.
 */

/** The sentence for a cross-cutting cause, or undefined for a per-rpc arm. */
export function refusalSentence(
  rpcName: string,
  cause: { case: string; value?: unknown },
): string | undefined {
  switch (cause.case) {
    case "unknownWorkspace":
      return "that workspace is not in the daemon's registry";
    case "workspaceRefMismatch": {
      const dir = (cause.value as { registryDir?: string } | undefined)?.registryDir;
      return dir === undefined || dir === ""
        ? "that workspace's directory disagrees with the registry's"
        : `that workspace's directory disagrees with the registry's (${dir})`;
    }
    case "transferringAway": {
      const address = (cause.value as { address?: string } | undefined)?.address;
      return address === undefined || address === ""
        ? "this daemon released that workspace to a successor"
        : `this daemon released that workspace to ${address}`;
    }
    case "notYetAdopted":
      return "a joining daemon has not finished adopting that workspace";
    default:
      return undefined;
  }
}
