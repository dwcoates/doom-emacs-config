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
