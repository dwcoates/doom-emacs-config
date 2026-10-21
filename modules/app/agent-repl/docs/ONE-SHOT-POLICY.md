# A repository's own one-shot policy

**Every repository states its own policy, in its own tree.** Owner ruling,
2026-09-12 (`docs/REALTEST-JUDGEMENT-CALLS.md`, "one-shot policy is the
repository's").

A one-shot workspace is fire-and-forget: the daemon wraps your prompt in your
repository's autonomous preamble and appends, behind one literal framing
sentence, the daemon's own COMPLETION DIRECTIVE, which tells the agent to run
`agent-repld workspace finished here` when the work is done. WHAT FINISHING
DOES is a policy decision about your repository, stated in its
`.agent-repl/deploy.yaml` (the deploy config `/agent-repl-workspaces`
documents): whether a local merge lands the work, whether a pull request is
opened, what runs after.

**There is no finish choice, and the daemon performs no finish action** (owner
ruling, 2026-09-12). The directive is concatenated to the commission as

```
when you're all done, please do the following postprocessing directive: <the daemon's directive>
```

and the AGENT carries it out. Nothing in the daemon merges, opens a pull
request, or sends a follow-up when the turn concludes: the agent's
`workspace finished` call queues the deploy its repository configured.

So a repository writes its preamble itself, in

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

The placeholder rules are the corpus's: every placeholder a file declares in
its header must appear in its body, and none may be invented. Copy the
corresponding corpus file as a starting point.

The completion directive is the daemon's own
(`modules/app/agent-repl/prompts/oneshot-completion-directive.md`) and is the
same for every repository; a repository states none. The prompts a deploy runs
before or after it lands are prompt hooks in the repository's deploy config,
not files here.

## When they run

- **The one-shot briefs** are read at composition time, so an edit takes effect
  on the very next one-shot. Their ABSENCE is detected at create time: the
  create is refused, no branch, worktree, workspace or prompt is made, and the
  refusal names the directory and the missing file. The preamble is required
  of every one-shot; the set does not vary.

## Where to look when it does not behave

Every one-shot create records the source it chose:

```sh
modules/app/agent-repl/bin/logs.sh --all --runtime daemon \
  --fields operation,message,repository_root,source,policy_dir
```

The record's operation is `daemon.workspace.oneshot_policy_source` and its
`source` is `corpus` or `repository`.
