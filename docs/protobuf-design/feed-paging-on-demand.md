# Feed paging on demand

## Problem

The daemon replays up to 200 history entries into memory every time it opens
a session watch (`sessionwatcher.openingPageSize`), and then keeps every live
entry since, whether or not any webapp shows them. The webapp shows the
newest page (50 rows) and walks older one page per click, but the daemon can
only serve what it already holds: it never asks the shim for older history
(`ReadHistory` exists and is never called), and a walk past the replay ends
in the "history replay stopped" refusal. The owner wants the shim, the store
and the daemon to hold and pull only the pages a webapp asked for.

## Settled with the owner (2026-10-02), before the design round opens

1. ONE page size across the whole stack: 50 STORE ENTRIES. Nothing else
   (watch opening, daemon feed, webapp) picks its own budget. A page may draw
   fewer than 50 rows, because entries coalesce into rows.
2. Opening a session watch replays NO history: the watch carries only what is
   written after the newest entry the daemon already holds (`known_through`).
3. The daemon loads history only on a webapp's request: opening a feed loads
   its newest page; "older" loads the next page back. A page already in
   daemon memory (e.g. from an earlier webapp instance) is served from
   memory; anything older is fetched daemon → shim → store.
4. The daemon keeps its in-memory feed (it owns daemon-only rows: cold gate,
   merge rows, faults), but holds only requested pages, the live tail, and
   daemon-only rows. A fully stateless pass-through was considered and set
   aside because of the daemon-only rows.
5. A row whose starting entry is on a page not yet loaded is not drawn until
   that page is loaded (a tool result whose call is older, a prompt's answers
   whose prompt is older).
6. The "history replay stopped" refusal goes away: older history is always
   reachable down to the conversation's start.
7. Daemon features that read held rows work over loaded pages only:
   response/prompt stepping loads the next older page when it runs past the
   oldest loaded row; rollback offers prompts from loaded pages.
8. Clicking an expanded-footer item (detached subagent, shell, monitor) that
   is not in the loaded feed uses its OWN RPC: the daemon locates the entry
   (asking the shim/store), loads EVERY intermediate page from the oldest
   loaded page down to the one holding it, and streams those pages to the
   webapp itself. If the entry cannot be found, the RPC answers a typed
   not-found, which the footer's activity line surfaces as an error.

## Carried in from vendor-start resilience (open)

- After a process-group kill with no teardown, the next shim's reconciliation
  re-announces spool-backed shells as live. Proposed: the daemon states the
  predecessor's fate in StartSession, and a "killed with its process" lost
  cause. Open: whether Claude Code background shells share the shim's process
  group; whether a restart's kill should draw as `cancelled` rather than lost.
