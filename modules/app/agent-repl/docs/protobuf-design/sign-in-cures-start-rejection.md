# A sign-in cures a start the vendor rejected for the credential

On 2026-11-11 the owner hit "Failed to authenticate: OAuth session expired and
could not be refreshed" while answering a cold gate with a compaction
(workspace `8a20a7203ea04158`). The summarizing session's error result made the
shim reject the StartSession, the daemon filed `vendor_start_rejected` and
`cold_gate_reopen_failed`, and nothing offered a sign-in: the sign-in action
existed only on TURN failures (`sign-in-cures-auth-failure.md`). The owner
asked that this case "result in an automatic authentication option like is
done logging in via the topbar", and that the work land directly on master.

Designed autonomously (the owner did not ask to co-design).

## Assumptions (autonomous mode)

- **Objective.** Every bring-up the vendor rejects because the credential was
  refused offers the sign-in automatically, through the page's one login
  overlay, and a completed sign-in for that account root cures it.
- **Systems.** shim (classifies the refusal), daemon (fault, footer, cold-gate
  card, sign-in fan-out, retry), webapp (draws the actions). Emacs decodes
  none of the touched messages and is untouched.
- **Non-additive changes.** None on the wire. In the daemon, the
  `vendor_start_rejected` footer line no longer says "restart: SPC o C-c" when
  a sign-in cures it; it says to sign in.

## Core design principles

- **One predicate decides what a sign-in cures.** `ladder.ApiFailureCuredBySignIn`
  (authentication_failed, oauth_org_not_allowed) decides it for a start exactly
  as for a turn, so the two paths cannot disagree about which refusals a
  sign-in answers. Consequence: the shim ships the typed `ApiRequestFailed`,
  never a pre-decided boolean.

## Landed changes

### `shim.v1.StartSessionVendorStartRejected.api_failure` (field 1)

- **What.** The rejected arm carries the vendor API's refusal, typed as
  `conversation.v1.ApiRequestFailed`, when the rejection was an API error
  result (the opening's, or the cold gate compaction's summarizer's).
- **Why.** The daemon received only the detail string, which it may not parse
  (the shim labels, the daemon never re-derives from `detail`).
- **Consequences.**
  - The shim builds the kind from the result's status, and when no status was
    stated, from the sentence (`classifyVendorApiFailure`): an
    "auth/credential/oauth" sentence reads as `authentication_failed`. A 403
    stays `permission_denied`, which no sign-in cures. Accepted cost: the
    no-status case is a sentence reading, the same one the shim's existing
    `shim.vendor.auth_rejected` diagnostic already relies on.
  - Only REJECTED verdicts carry it. A network-worded failure stays
    retryable/network and carries nothing.
  - Producers: `settleStartOnErrorResult` (session.ts) and the compaction's
    `errorResult` branch in `compact` (session.ts), through
    `VendorStartError.apiFailure` and the remediation's failed outcome to
    `refuseVendorStart`.

### `frontend.v1.FooterStatusActivityVendorStart.sign_in` (field 2) and `FooterVendorStartSignIn`

- **What.** The footer's vendor-start line gains a presence-only sign-in
  action, set while a sign-in-curable `vendor_rejection` stands.
- **Why.** A plain bring-up rejection (cold start, relaunch, restart) has no
  feed row; the footer's vendor-start line is where the refusal is read.
- **Consequences.**
  - The daemon records the curability on the fault as evidence
    (`health.EvidenceSignInCures`), so the footer line, its text and the
    sign-in fan-out read one recorded fact.
  - The line's text says "sign in to retry" instead of the restart key.
  - A completed sign-in for the root RESTARTS each such workspace that has no
    standing cold gate (the same path `SPC o C-c` drives, Restart verb), which
    retires the fault through the started session's edge.

### `frontend.v1.FeedColdGateStanding.sign_in` (field 6), `FeedColdGateSignIn`, `FeedColdGateSignInCause`

- **What.** A cold gate re-raised because its answer's re-open was rejected
  for the credential carries a notice: the vendor's sentence and the sign-in
  action (`FeedOutcomeSignIn`, the action the marker and the broken bubble
  already offer).
- **Why.** In the owner's case the footer draws `cold_gate_reopen_failed`
  (agent-repl fault ranks above vendor fault), so a footer-only action would
  not be seen; the re-raised gate card is where the user is asked to act.
- **Consequences.**
  - A completed sign-in removes the notice and leaves the gate standing: the
    gate is the user's choice, so the daemon does not re-spend the answer on
    its own (a compaction spends tokens).
  - The sign-in also closes the standing `vendor_start_rejected` fault for that
    workspace, which the sign-in answered. `cold_gate_reopen_failed` stays until
    a session starts, because the re-open did fail.
  - `FeedOutcomeSignIn` is reused rather than respelled: one sign-in action,
    one webapp drawing (`drawSignInAction`).

## Sweep

No declaration is obviated: every new field is additive and nothing stopped
being referenced.

## Correction (2026-11-11): the login opens by itself; the two actions are retired

- **What was wrong.** The owner asked for the sign-in to happen
  AUTOMATICALLY ("an automatic authentication option like is done logging in
  via the topbar"); the first landing read "option" as an action to click and
  added two (`FooterStatusActivityVendorStart.sign_in`,
  `FeedColdGateStanding.sign_in`). The owner: "i did not ask for a button ...
  i asked for it to automatically pop up when authentication is determined to
  be needed again", and "lose whatever button you added". Root cause: the
  request's operative word ("automatic") was weighed below its noun
  ("option"), and the assumption was recorded instead of being checked.
- **Retired.** Both fields and their messages (`FooterVendorStartSignIn`,
  `FeedColdGateSignIn`, `FeedColdGateSignInCause`); tags 2 and 6 reserved.
  The pre-existing turn sign-in actions (marker, broken bubble; owner request
  2026-10-12) are untouched.
- **Added: `frontend.v1.TopbarAccount.sign_in_request` (field 4),
  `TopbarAccountSignInRequest`.** The daemon raises a request, with a fresh
  id, on every strip spending as the account the moment it determines a
  sign-in is needed: a turn whose failure `ladder.FailureCuredBySignIn`
  names, or a start rejected for the credential (the shim's typed
  `api_failure`). The page opens the topbar's own login overlay by itself,
  once per id. The request clears when that account's login flow ends,
  signed in or closed by the user, so a dismissed login does not reopen until
  the next determination.
- **Why on the topbar.** The account cell is the component that owns the
  login flow (its click is `OpenLogin`), and every workspace page draws it,
  so whichever page the reader is on opens the one login.
- **Kept.** A completed sign-in still retries a rejected start (Restart), and
  under a standing cold gate closes the rejection and leaves the gate for the
  user's answer. The footer's line still says "sign in to retry", which is
  text, not an action.
