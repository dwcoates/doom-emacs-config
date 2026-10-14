# Merge version increment

agent-repl is versioned with semantic-version git tags on master. Every
merge the queue lands on agent-repl's own master must declare how it moves
the version and carry one changelog line, so the version can never fall
behind what landed. The daemon enforces this when the merge is requested,
and the queue creates the tag in the same step that lands the merge.

## Core design principles

### Agents never increment the major version
- **Principle (owner, 2026-10-13):** only a human increments the major
  version. "Major" is the owner's judgment of the product's maturity, not
  the usual semver meaning of "a breaking change". Breaking changes made
  through the queue are minor increments.
- **Consequences for the contract:** the merge request's increment offers
  only minor and patch. Agents cannot ask for a major increment, because the
  request has no field for one.
- **Reopens:** nothing landed.
- **Does not claim:** that a human bumps major through the queue. The human
  path is a hand-made annotated tag on master (see Landed changes).

## Landed changes

### Tag format: bare `MAJOR.MINOR.PATCH`, no `v` prefix (2026-10-13)
- **What:** product version tags read `0.0.1`, not `v0.0.1`. The owner's
  seed tag `v0.0.1` (lightweight, on `1711e67a3`) was replaced by an
  annotated `0.0.1` on the same commit.
- **Why:** the owner prefers no prefix and left the call to the lead if a
  real reason favored `v`.
- **Why the `v` lost:** Go's module tooling is the only consumer here that
  requires `v`. It reads tags only as module versions, and this repository's
  Go modules live in subdirectories, so their version tags would have to be
  path-prefixed anyway (`proto/gen/go/v0.1.0`). A product tag can never
  serve as a Go module version, so the prefix buys nothing. MELPA Stable
  accepts either form.
- **Consequence:** the queue and the build look for the highest tag that
  parses as bare semver among the tags reachable from master, and ignore
  every other tag (`start/*`, `wip-*` and so on).

### The human major increment is a hand-made tag (2026-10-13)
- **What:** a human bumps major by creating an annotated tag
  `<major+1>.0.0` on master's tip. The tag adds no commit to master, so it
  does not violate the rule that master moves only through the queue. The
  queue's next landing then counts from it.

### The changelog lives in the tag annotations (2026-10-13)
- **What:** each increment carries one changelog line. The queue writes it as
  the annotation message of the tag it creates, so there is no changelog file
  for a landing to edit (or conflict over). A `CHANGELOG.md` is generated
  from the annotations when one is wanted.

### The request's increment and its two refusals (2026-10-13)
- **What:** `agentrepl.v1.MergeWorkspaceRequest.version_increment`
  (`MergeWorkspaceVersionIncrement`: a minor-or-patch oneof plus
  `changelog_line`), and the error arms `version_increment_required` and
  `version_increment_unexpected` (the latter's reason: foreign repository, a
  target that is not the default branch, or a one-shot already merged
  upstream). The owner's sketch named the message `VersionIncrement` with
  arms `SemverMinor` and `SemverPatch`; the landed names carry the
  endpoint's `MergeWorkspace` prefix, as every message in that file does.
- **Why:** the owner wants the version always moved by the merge queue, with
  a changelog line, and never by a forgotten step.
- **Required only for an agent's ask (owner, 2026-10-13: "don't worry about
  human merges ... version only increments on merge queue landings"):** a
  merge onto agent-repl's master that the AGENT path asked for (the
  command-file ingress the merge-queue skill writes, `merge.RequestedByAgent`)
  is refused without an increment. A merge the user asks for over the rpc
  (Emacs, the webapp) may carry one and otherwise lands untagged. The
  "unexpected" refusal applies to both paths.
- **Agent's ask travels as a command-file entry, not this rpc:** the
  command-file `Entry` (daemon/internal/commandfile/entry.go) carries the same
  two facts in JSON; the ingress maps it onto the same `merge.Request` the rpc
  builds, so both paths share one validation site in `merge.Enqueue`.
- **Consequences:**
  - The increment is persisted with the queue row (`merge_queue`, layout 48)
    and carried across the async hand-off to the merge workspace's own row,
    because the version is computed when the merge LANDS.
  - The tag is created right after the fast-forward, inside the committing
    step; a restart that finds the target on the recorded merge commit
    re-enters the "landed" path, which creates the tag if it is missing.
    A tag already on the merge commit is the step having finished before the
    restart. A tag of the computed name on any OTHER commit is a fault: the
    landing stands (the target already moved), the fault is logged at ERROR,
    and the bubble draws no version.
  - The landed version rides `merge.outcome` and its persisted `outcomeDoc`,
    so a restart between the landing and the bubble's terminal still draws
    it.

### The bubble head's version (2026-10-13)
- **What:** `frontend.v1.FeedMergeHead.version` (`FeedMergeVersion`: leading
  text, the incremented number, optional trailing text).
- **Why (owner):** once a merge into agent-repl's master lands and its bubble
  folds, the visible head shows the new version with the incremented number
  colored green.
- **Consequence:** the daemon splits the version; the webapp draws the three
  runs verbatim and colors only the incremented one, deriving nothing.
