# API retry resolves the blocked rung

## Problem

While the vendor retries a failing API call mid-turn (DNS failure, a revoked
token, anything the vendor answers with `attempt N of M, next attempt at T`),
the workspace stays on the `thinking` rung and draws red on the footer, the
sidebar and the tab bar. The workspace is not usable in that state: nothing
advances until the API answers. The owner wants it blue (unusable) while the
API fails, red for the moment a new prompt is registered, and blue again as
soon as the API fails once more.

The footer already carries the retry as a `working` salient line
(`FooterStatusActivityRetrying`), but the `blocked` arm's salient oneof has no
retrying line, so moving the claim to `blocked` today would lose the attempt
count and the countdown to the next attempt.

## Landed changes
