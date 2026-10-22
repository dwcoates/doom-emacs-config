# Image overlay over the feed, and held-card thumbnails

## Problem

A pasted image is drawn as a thumbnail in the Emacs input window and in the
feed's prompt bubbles. The owner wants a click on an image -- in the Emacs
input window or anywhere in the webapp -- to open it LARGE, as an overlay over
the feed, instead of opening the OS image viewer. The held-prompt tray must
also draw a held prompt's images as thumbnails once its card is expanded,
with the same click behavior; today it can only name a host-path image,
because it receives the raw `conversation.v1.UserSaid` and the webview cannot
load a host path.

## Context (step 1)

- **Ultimate objective (owner, 2026-10-17):** clicking an image "either in
  emacs or in webapp" opens it "in the browser as an overlay over the feed";
  a held prompt shows its images "only when expanded, same click rules apply".
- **What is being built:** a per-workspace image overlay drawn by the webapp
  over the feed, opened from Emacs or from the webapp; daemon-resolved image
  thumbnails on held cards.
- **Frontend component:** yes -- figma→idl governs the overlay and the held
  card's image blocks.
- **Systems involved (proposed):** `frontend.v1` and `agentrepl.v1` protos;
  the daemon (held-card block resolution, overlay state, the requests, the
  root feed watch push); the webapp (the overlay, clicks on prompt, held and
  tool-output images, closing); Emacs (a composer click sends the request).
- **Non-additive changes (proposed):** the Emacs composer click stops opening
  the OS viewer (`agent-repl--image-open` and its `open` / `xdg-open` use are
  removed; `sips` keeps the process boundary); the held card stops drawing
  images from the raw `said` and draws daemon-resolved blocks instead.

## Settled outside the contract (2026-10-17)

- Collapsed prompt bubbles and held cards draw no images; images appear only
  once the bubble is expanded (landed in the webapp as the expanded-only mark).
- Tool-output images (an image a tool call returned, drawn in its card) stay
  120px thumbnails.

## Landed changes

## Owner rulings (2026-10-22)

- How an overlay closes: the agent's call. Decided: a click on the overlay
  (image or backdrop) closes it, and so does Escape while the webapp has
  focus. A click on another image replaces it. No Emacs key closes it: a
  click on the overlay works whichever window has focus.
- An open overlay survives switching workspaces and coming back: yes.
- Size: just shy of the feed's streaming area (where the rows span, left to
  right) horizontally and vertically, sitting over the rows without fully
  obscuring them -- not a full-window viewer.
- The owner handed the whole design and implementation to the agent, with no
  further input, and asked for the branch to be rebased onto master first.

## Landed changes

### 1. The overlay component, its two requests and its frame (2026-10-22)

- **What:** `frontend.v1.FeedImageOverlay` (new file
  `feed_image_overlay.proto`) is the overlay's view: a `oneof state` of
  `closed` and `open`, the open arm carrying the `frontend.v1.FeedImageBlock`
  it draws. `agentrepl.v1.OpenFeedImage` opens an image (arm `composer`: a
  `conversation.v1.ImageBlock` from the Emacs input window; arm `drawn`: a
  `frontend.v1.FeedImageBlock` the webapp drew, echoed as served) and
  `agentrepl.v1.CloseFeedImage` closes it. `WatchFeedResponse.image_overlay`
  (tag 5) carries the overlay on the root feed's watch.
- **Why:** a click in Emacs and a click in the webapp are two producers of one
  state; holding it in the daemon, per workspace, is what lets an Emacs click
  open the webapp's overlay and lets the overlay survive a workspace switch.
- **Consequences:**
  - The daemon keeps a per-workspace overlay topic, modeled on the selection
    topic (`daemon/internal/server/select_feed_row.go` `selectionTopic`): it
    replays its latest value, so a root watch accepted late is told what is
    open. A workspace never touched publishes nothing; the webapp's default is
    closed.
  - The `drawn` arm is a typed echo of what the daemon served. The daemon
    opens only a source its image origin serves or a url of a scheme the feed
    would draw (`daemon/internal/resolve/feed/image.go` `safeImageSchemes`);
    anything else is the `not_drawn` refusal.
  - The `composer` arm is resolved by the same `feed.ImageResolver` the feed
    draws prompts with, so a path the overlay shows is registered with the
    image origin exactly once and served the same way.
  - The Emacs composer's click no longer opens the OS viewer:
    `agent-repl--image-open` and its `open` / `xdg-open` use are removed (the
    `sips` conversion keeps the process boundary).
- **Rejected:** a webapp-only overlay (an Emacs click could not reach it, and
  it would not survive a workspace switch); an ephemeral "show" event pushed
  to the page (lost whenever no page was attached at the moment of the click).

### 2. Held cards carry their drawn body (2026-10-22)

- **What:** `frontend.v1.HeldPrompt.body` (tag 26) and
  `frontend.v1.HeldIngressPrompt.body` (tag 4) are the
  `frontend.v1.FeedUserPromptBody` the card draws; `said` stays for edits and
  drops.
- **Why:** the tray could not draw a host-path image -- the webview has no
  filesystem -- so it named the file instead. The daemon resolves the body
  exactly as the prompt's feed row will be (`feed.DrawUserBlocks`), so the
  card shows what will land, drawn the way the feed draws it.
- **Consequences:**
  - A held card's reply quote is drawn from its parts, as the feed row's is,
    instead of verbatim; sentinel spans are stripped as on the row.
  - `holds.Prompts` / `holds.IngressPrompt` take the drawing function from the
    feed resolver, which owns `DrawUserBlocks`'s dependencies.
