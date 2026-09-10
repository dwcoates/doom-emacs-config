# Owner 8 (C22-C24, composer extras) -- live state

Branch `overhaul/int-play-08`, worktree `integration-agents/play-08`.
Rebased onto `overhaul/integration` c53e457a2 (one comment-only conflict in
lisp/clipboard-image.el, resolved by keeping integration's platform prose and
adding the marker-property paragraph).

## Landed this session (all committed, trailer on every commit)
- `feat(daemon/imageorigin)`: the daemon now serves the images a drawn feed
  refers to (`internal/imageorigin`, mounted at `/feed-images/`), and
  `feed.PathImageResolver` registers a prompt's `ImageBlock{path}` with it.
  Replaces `feed.UnproducedImageResolver` (deleted with its test). Unit tests:
  `internal/imageorigin/origin_test.go` (9), `internal/resolve/feed/image_test.go` (8).
  Integration test: `integration/image_origin_test.go` (3), SPEC section added.
- `test(e2e/playtest)`: C23 now drives the REAL verb -- a PNG is put on the
  world's X clipboard with `xclip` and `agent-repl-attach-clipboard-image` is
  invoked as a command; the captured bytes are compared against the clipboard's.
  The feed chip is an assertion (`img.prompt-block-image`, loaded at the
  attachment's natural width), not a reported class.
- `docs(daemon)`: ARCHITECTURE.md names the image origin.
- `fix(daemon/promptqueue)`: the mirror and the feed resolver drew the SAME
  user_prompt row from two implementations, and the mirror dropped every image
  block. Nothing brings a delivered user prompt back on the watch, so live an
  attached image was invisible for the whole session. One exported
  `feed.DrawUserBlocks` is now the only implementation and the queue is
  REQUIRED to hold the same image resolver. Unit tests: promptqueue
  deliver_test (2) + queue_test refusal case, feed image_test (3).

## Gates run so far (host)
- daemon `go build`/`go vet`/`go test ./internal/... ./cmd/...`: green.
- daemon integration suite (whole, -parallel 8): green.
- lisp aggregate: 3759/3759.
- e2e gofmt + vet (plain, playtest, integration tags): green.
- webapp typecheck: green.

## Runs
- Run 5 (pre-rebase, commit 44f6366d8): GREEN. Not counted -- the section has
  changed since.
- Sandbox image rebuild (another agent's, holding the suite slot) is in flight;
  it carries `xclip`, which C23 now needs.

## Filed (not mine to fix)
- Plan text says `SPC TAB e`/`E`; the product binds `SPC j e e`/`SPC j e E`.
- Substrate: the first capture in a world was blank in 3 of 3 pre-rebase runs.
  Integration's paint gate addresses the cause; re-check on the next run.

## Next
1. Wait out the image build, then run the three playbooks under the suite slot.
2. Run the daemon integration suite and the lisp aggregate.
3. Two consecutive green runs with every picture inspected.
4. Delete this file in the last commit.
