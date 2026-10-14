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
