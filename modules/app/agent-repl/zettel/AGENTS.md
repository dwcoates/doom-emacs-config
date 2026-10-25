# zettel/

zettel is agent-repl's knowledge graph: every question asked in a chat thread,
and its answer, as a node in a graph of questions asked of answers. This
directory is the zettel service, a singleton launchd-managed Go process that
alone owns zettel's SQLite database and serves `zettel.v1.Zettel`
(`proto/src/zettel/v1/`) over Connect on a unix socket. The daemon is its only
caller. The planning record is `docs/knowledge-graph/PLAN.md`; the contract's
design record is `docs/protobuf-design/zettel-phase1.md`.

## Glossary

Every term below is the one word for its concept, in conversation, docs and
code alike, with no synonyms in code. A known synonym is listed so it is
understood when it comes up in conversation. A code identifier is given where
the spoken term would clash in code.

| Term | Meaning | Known synonyms / code identifier |
|---|---|---|
| zettel | The knowledge graph system: this service, its database, and the graph it holds. Orthogonal to agent-repl, though it runs inside it. | |
| node | A node in the graph. Holds at least a question and its answer, plus what belongs with them (its parent, quote, sightings). | |
| thread | One chat: a root node and every node beneath it. To the daemon, a kind of workspace. | "chat" |
| parent | Where a node's question came from: another node in the same thread (with the revision asked of), or the start of the thread. A node has at most one, so a thread is a tree. | "provenance" |
| quote | The exact words highlighted when the question was asked, stored with the parent. | |
| context | The text around a highlight that gives it meaning. | code: `questionContext` (`question_context` in protos) |
| revision | One frozen version of a node's answer. Revision 1 is the answer as first given; a correction adds the next. | |
| premise | A node that is conceptual groundwork for another, regardless of which was asked first (phase 2). | |
| exchange | One answered question being recorded as a node, keyed by the daemon turn it was. Until the answer is known, the question and its origin ride the daemon's own record of the prompt; zettel holds answered nodes only. | |
| sighting | One time a node's answer bubble came fully onto the screen in a consumer UI. | |
| visit | One stretch during which a parent stays on the chat's shown path; each sibling's branch is picked once per visit. | |
| contributor | A UI that adds to the graph (the chat, agent-repl's feed, a book reader). | |
| consumer | A UI that renders the graph (the chat, flash cards). | |
| relay | The service that will carry remote clients' encrypted traffic to the workstation (later phase). | |

## Rules

- **The graph is the user's data and is carried forward, never thrown
  away.** Unlike the record store, a layout change is a migration step in
  `internal/graph/db.go`, and a file stamped newer than the build is refused,
  never rewritten. The database lives at `~/zettel/zettel.db`, beside the
  threads' directories.
- **One writer.** Every write is one IMMEDIATE transaction on the one write
  connection, so a check and the write it guards (a thread starts once, a
  parent exists in the thread) never interleave with another write.
- **One mapping.** `internal/server/mapping.go` is the only translation
  between `zettel.v1` and the graph's Go types; `mapping_test.go` fails when a
  protocol field is added and not mapped.
- **Logging.** Every record goes through `internal/zlog` (runtime `zettel`).
  The graph logs each storage failure where it happens; the server logs each
  refused request at info and never re-logs a storage failure.
- **Tests.** Unit tests beside the code prove every graph rule and every
  response arm; `integration/` boots the real binary only for what needs the
  process (the wire, a restart keeping the graph, SIGTERM, the singleton
  socket). No other agent-repl system runs in either tier.

Run the suites with:

```bash
../bin/background.sh go test ./...
```
