/**
 * pull-request-owner — A SHIFT-CLICK ON A PULL REQUEST LINK SWITCHES TO THE
 * WORKSPACE THAT OWNS IT rather than opening the link in the browser.
 *
 * THE DAEMON DECIDES. The page asks `FindPullRequestOwner` for the link's url
 * and never reads a GitHub address itself: the daemon says whether the url
 * names a pull request and which workspace's canonical owned link it is. An
 * owner is reached through the feed's one jump (feed/merge/jump.ts), chosen by
 * its standing as a workspace mention's is: an open owner is selected, a
 * closed one reopened and then selected. A pull request nobody owns is said at
 * the link, and the link opens nothing. A url that names no pull request is
 * answered back to the caller, which opens it as a plain click would.
 *
 * Every refusal is drawn INSIDE the link, in the one class, so the next
 * shift-click on it replaces the last one rather than stacking them.
 */
import { createLogger } from "../log.js";
import { childrenMatching } from "../dom.js";
import { isMalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { AppContext } from "../rpc/context.js";
import { FindPullRequestOwnerResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_find_pull_request_owner_pb";
import { drawJumpRefusal, jumpForStanding, type JumpSite } from "./merge/jump.js";

const log = createLogger("webapp.feed.pull-request-owner");

/** Where the owner's jump is recorded and its refusal drawn. */
export const PULL_REQUEST_OWNER_JUMP: JumpSite = {
  describe: "a pull request link",
  operation: "feed.pull-request-owner",
  refusalClass: "pull-request-owner-refusal",
};

/**
 * What a shift-click did: switched (or drew why not), or nothing, because the
 * url names no pull request and the caller opens it as a plain click.
 */
export type PullRequestOwnerClick = "handled" | "notPullRequest";

/**
 * Switch to the workspace owning the pull request URL names, from ANCHOR, the
 * link clicked. A malformed answer travels up, for the caller's
 * `guardMalformed`.
 */
export async function jumpToPullRequestOwner(
  ctx: AppContext,
  anchor: HTMLElement,
  url: string,
): Promise<PullRequestOwnerClick> {
  const site = PULL_REQUEST_OWNER_JUMP;
  for (const stale of childrenMatching(anchor, `.${site.refusalClass}`)) stale.remove();
  let answer;
  try {
    const response = await callUnary(
      ctx,
      "FindPullRequestOwner",
      (client) => client.findPullRequestOwner({ url }),
      FindPullRequestOwnerResponseSchema,
    );
    const result = requireCase(response.result, "FindPullRequestOwnerResponse.result");
    answer = requireCase(result.value.answer, "FindPullRequestOwnerSuccess.answer");
  } catch (err) {
    if (isMalformedView(err)) throw err;
    // callUnary recorded the transport failure once, at ERROR, and told the
    // footer; the link says so where the user clicked.
    anchor.append(drawJumpRefusal(site, "transport", "the daemon could not be reached"));
    return "handled";
  }
  switch (answer.case) {
    case "owned": {
      const owned = answer.value;
      const workspace = requireMessage(owned.workspace, "FindPullRequestOwnerOwned.workspace");
      const standing = requireCase(owned.standing, "FindPullRequestOwnerOwned.standing").case;
      await jumpForStanding(standing, "FindPullRequestOwnerOwned.standing")(anchor, ctx, workspace, site);
      return "handled";
    }
    case "unowned":
      log.info("no workspace owns the pull request a link names; nothing is switched to", {
        operation: `${site.operation}-unowned`,
        context: { url },
      });
      anchor.append(drawJumpRefusal(site, "unowned", "no workspace owns this pull request"));
      return "handled";
    case "notPullRequest":
      return "notPullRequest";
    default: {
      const other: { case: string } = answer;
      return unreachableArm("FindPullRequestOwnerSuccess.answer", other.case);
    }
  }
}
