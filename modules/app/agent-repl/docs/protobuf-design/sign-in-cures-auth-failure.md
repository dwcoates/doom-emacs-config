# A completed sign-in cures an auth failure

When a turn ends on "vendor error · authentication failed", the feed draws two
things: the red-bordered broken response bubble the failure cut short, and the
outcome marker whose expansion offers "sign in". The owner asked (2026-10-12)
that the sign-in action also sit INSIDE the red-bordered bubble, and that once
the user signs in through that flow and the login dialog closes, the bubble and
its sign-in action leave the feed and the workspace status returns from its
fault to idle/ready.

The owner asked for the work to proceed without waiting for further
instructions, so the skill's interactive selection gates were not run; the one
contract change is a single additive optional field.

## Landed changes

### `frontend.v1.FeedResponseError.sign_in` (field 2)

- **What changed.** The broken state of a response bubble gains an optional
  sign-in action, typed as the marker's existing `frontend.v1.FeedOutcomeSignIn`
  (an empty presence-only action), not a new type.
- **Why.** The owner wants the action reachable from the bubble the failure cut
  short, without first opening the marker's expansion.
- **Consequences.**
  - The marker keeps its own sign-in action. The owner's "put it inside the
    bubble, too" reads as an addition; a turn that failed with no response
    bubble still has the marker's action.
  - The bubble is drawn before the turn's terminal, so the daemon stamps the
    action when the terminal names a sign-in-curable cause and re-pushes the
    bubble (`resolve/feed` keeps the turns, like `finalAnswerSeen`).
  - Which causes qualify is one predicate,
    `ladder.ApiFailureCuredBySignIn` (authentication_failed,
    oauth_org_not_allowed), shared by the marker, the bubble, the footer's
    `auth` step and the sign-in lift.
  - No contract change carries the lift itself: the feed retires the rows
    through its existing removal publication, and the footer and roster
    re-resolve their status. The retirement is recorded durably by turn (the
    rolled-back-turns pattern), so a daemon restart does not redraw it.
