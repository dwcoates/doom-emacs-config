# A repository's own one-shot and merge policy

**Every repository states its own policy, in its own tree.** Owner ruling,
2026-09-12 (`docs/REALTEST-JUDGEMENT-CALLS.md`, "one-shot policy is the
repository's").

A one-shot workspace is fire-and-forget: the daemon wraps your prompt in an
autonomous preamble and appends, behind one literal framing sentence, your
repository's COMPLETION DIRECTIVE — the one plain-English statement of what is
to be done when the work is done. What that directive SAYS is a policy decision
about your repository: what "done" means there, which command finishes the
work, whether a merge or a pull request lands it.

**There is no finish choice, and the daemon performs no finish action** (owner
ruling, 2026-09-12). The directive is concatenated to the commission as

```
when you're all done, please do the following postprocessing directive: <your directive>
```

and the AGENT carries it out. Nothing in the daemon merges, opens a pull
request, or sends a follow-up when the turn concludes.

So a repository writes them itself, in

```
<repository main checkout root>/.agent-repl/prompts/
```

The daemon's own prompt corpus (`modules/app/agent-repl/prompts/`) is the
policy of **exactly one** repository — the one this checkout lives in — and is
**never** a fallback for any other. A repository that states no policy does not
inherit doom's: the daemon refuses the one-shot create, and the editor draws
the refusal as a warning naming this directory and the files it needs.

## The files

Same format as the corpus (see `prompts/README.md`): a first-line header
comment, then the text.

| file | required | what it is |
| --- | --- | --- |
| `workspace-autonomous-preamble.md` | every one-shot | the "do not wait for further instructions" preamble the one-shot's first message opens with |
| `oneshot-completion-directive.md` | every one-shot | the plain-English completion directive, submitted verbatim; it declares NO placeholders |
| `merge-before.md` | never | runs as the before-merge action of every merge of a workspace in this repository |
| `merge-after.md` | never | runs as the after-merge action of the same |

The placeholder rules are the corpus's: every placeholder a file declares in
its header must appear in its body, and none may be invented. Copy the
corresponding corpus file as a starting point.

`oneshot-completion-directive.md` declares **no** placeholders and is submitted
verbatim, exactly as the merge briefs are: the daemon has nothing to fill a
placeholder in your own completion statement from, and one that declares any
fails the create.

`merge-before.md` and `merge-after.md` declare **no** placeholders — they are
submitted verbatim — and both are optional. A repository that states neither
has no configured merge action, which is what every repository had before this
policy existed.

## When they run

- **The one-shot briefs** are read at composition time, so an edit takes effect
  on the very next one-shot. Their ABSENCE is detected at create time: the
  create is refused, no branch, worktree, workspace or prompt is made, and the
  refusal names the directory and the missing files. Both are required of
  every one-shot; the set does not vary.
- **The merge briefs** fill an EMPTY slot. A workspace created with explicit
  merge actions runs those; the repository's file is the default for workspaces
  that configured none. A brief that is present but will not read fails the
  merge rather than being skipped.

## Where to look when it does not behave

Every one-shot create records the source it chose:

```sh
modules/app/agent-repl/bin/logs.sh --all --runtime daemon \
  --fields operation,message,repository_root,source,policy_dir
```

The record's operation is `daemon.workspace.oneshot_policy_source` and its
`source` is `corpus` or `repository`.
