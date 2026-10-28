# The daemon reads history from the store, read-only

Today a workspace's conversation rows reach the daemon only through that
workspace's shim (`shim.v1.ReadHistory`), which translates the store's pages
into `conversation.v1.HistoryPage`. So reading a parked or cold-gated
workspace's history means starting its shim. That forced the restore-on-view
machinery (`cachelapse` `RestoreParked`, the viewed and deferred bookkeeping),
it is why a deploy brought shims back as every page reloaded, and it is the
one reason a workspace at its cold gate cannot simply have no shim. The owner
wants a shim to run only when a session must: a turn, a keep-alive, or an
answered cold gate. Viewing anything never starts one.

## Core design principles

- **The daemon's relationship with the store is, and remains, READ-ONLY**
  (owner ruling, 2026-10-27).
  - *Consequences for the contract:* the daemon's view of the store is its own
    proto package, `storeread.v1`, which holds no write verb and imports
    nothing from `store.v1`, so no storage type and no write request can reach
    the daemon through it. The store serves it on its own socket, with its own
    handler holding only the store's read-only pool (SQLite `query_only`), so
    a client on that socket cannot write whatever it sends. The proto build's
    isolation gate already refuses any daemon import of `store.v1`; it is
    extended to refuse a `storeread.v1` file importing anything but
    `conversation.v1`.
  - *Decisions it reopens:* `store.v1/service.proto`'s header ("callers are the
    shim and the sidecar, never the daemon; the daemon's read path is shim.v1")
    is restated: the daemon still never calls `store.v1`, and reads history
    through `storeread.v1`.
  - *What it does not claim:* it does not make the store's data immutable
    (producers still write through `store.v1.WriteBatch`), and it says nothing
    about retention. Pruning a closed workspace's book, when it comes, is the
    store's own retention, never a daemon delete. Facts the daemon wants to
    attach to rows (merge reports, notices) stay in the daemon's own state.
- **A shim runs only when a session must** (owner ruling, 2026-10-27). Viewing,
  paging, a deploy's page reloads and a cold gate standing start nothing.
  Consequence for this change: every history read the daemon makes goes to the
  store, never to a shim, so nothing about reading needs a shim up.

## Iteration sequence

Decided by the orchestrator under the standing ruling to carry contract
changes through and report after (2026-10-13):

1. The read-only package and its one rpc, `storeread.v1.StoreRead.ReadHistoryPage`.
2. The shim stating the workspace's book on every StartSession answer.
3. Retiring `shim.v1.ReadHistory` once nothing calls it.

## Landed changes

### 1. `storeread.v1`: the store's read-only face, one rpc

- **What changed.** A new proto package, `storeread.v1`, with one service,
  `StoreRead`, and one rpc, `ReadHistoryPage`: one page of one agent's book,
  as the newest page, the page below a served pointer, or the book as it stood
  at an instant, answered as `conversation.v1.HistoryPage` with a typed
  failure. The proto build discovers the package (`proto/Makefile`), and the
  isolation gate (`scripts/check-conversation-isolation.sh`) gains invariant
  I8: a `storeread/v1` file imports only `conversation/v1` and `storeread/v1`,
  with three self-test rows (a `store.v1` import refused, any other import
  refused, the package missing is a setup failure). `store.v1/service.proto`'s
  header now names `storeread.v1` as the daemon's one way into the store.
- **Why.** The owner wants no shim started just to show a conversation, and
  ruled that the daemon's relationship with the store must be and remain
  read-only, enforced structurally.
- **Why a package of its own rather than a second service in `store.v1`.**
  One proto package generates into one Go package, so a daemon importing a
  read service declared in `store.v1` would also get the `ShimStore` client and
  `WriteBatch`. A separate package is what lets the existing gate keep
  refusing every `store.v1` import from the daemon.
- **Why `newest` carries `stated` or `named`.** The store registers a book on
  its first row, so a session that has written nothing yet has no book, and
  the store refuses an unregistered agent `unknown_agent` unless the caller
  vouches for it (`OpenAgentSessionRequest.vouched`). The shim used to vouch
  for agents it had minted. The daemon's equivalent is the book a shim STATED
  for the workspace (change 2), so a stated read serves the absent book empty
  and a merely named one (a subagent, a fork's parent) is refused. A bool
  would select the interpretation of the read, so it is a two-arm oneof.
- **Accepted consequences.**
  - The store page to history page translation moves into the store, in Go.
    The shim keeps its own TypeScript translation of lines for its live
    `WatchAgent` tail (`agent-shim/claude/shim/src/store/reader.ts`,
    `toHistoryEntryAt` and its callers), so the line mapping exists in both
    languages until the tail moves too. Pages and tail must map a stored line
    identically; a cross-language golden test guards the pair until one copy
    is retired.
  - The store gains a second listening socket, which its launchd plist, the
    deploy, and the test harnesses that start a private store must name.

### 2. `shim.v1.StartSessionResponse.book`

- **What changed.** `StartSessionResponse` gains `optional
  conversation.v1.AgentId book = 3`, beside the outcome oneof, stated on every
  answer.
- **Why.** The daemon has no record of which book holds a workspace's
  conversation: the book is the ORIGINAL vendor session's identity, kept in the
  shim's own state directory (`agent-id.json`, written by
  `engine/identity.ts`), and `wsm.Session.VendorSessionID` is the CURRENT one,
  which a `/clear` rotation moves off the book. Verified by reading
  `engine/turn.ts` `readHistory` (it reads `identity().agentId` or the
  persisted record, never the current vendor id) and `convert/ids.ts`
  `mainAgentId`. The store keys no book by workspace (its `entry` and `agent`
  tables, `shim-store/internal/db/db.go`), so it cannot answer either.
- **Why on the response, not on `SessionStarted` or `SessionCold`.** The book
  is a fact of the workspace, not of the outcome: the cold gate (a failure
  arm) needs it most, since that is the workspace that must show its
  conversation with no shim, and a vendor that would not start still leaves a
  book the user wants to read.
- **Accepted consequences.** A workspace the daemon has not seen start since
  this lands has no recorded book, so its history is unreadable without a shim
  until its next start (a keep-alive restore, a prompt, a gate answer). The
  daemon keeps the shim read as the fallback for exactly that case until every
  workspace has started once under the new build, and logs each use at INFO so
  the fallback's retirement is evidenced.

## Deferred, by name

- **Retiring `shim.v1.ReadHistory`** (iteration step 3). Done 2026-10-28: a
  workspace with no recorded book takes the book its shim persisted
  (`shimidentity`, `agent-id.json`) and records it, so no read goes through a
  shim. The fallback, the parked-session restore it asked for (`cachelapse`
  `RestoreParked`, `cachelapse/viewed.go`) and the rpc retired together.
- **No shim at the cold gate.** Reading history from the store is what lets a
  gated workspace show its conversation with no shim. Standing the shim down
  at the gate also needs the gate itself to outlive the shim: its facts
  (vendor session id, the shim's `SessionCold`, the account root) recorded
  durably and raised again with no shim, and an answer that spawns the shim it
  resumes through. Done by the follow-up change that records the gate
  (`wsm` `cold_gate`, daemon/AGENTS.md "A workspace at its cold gate runs no
  shim"); it needed no contract change.
