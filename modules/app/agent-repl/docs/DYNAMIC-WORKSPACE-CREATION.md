# Dynamically created workspaces: the backend design

The backend for the three DYNAMIC creation modes — plain dynamic create,
dynamic fork, and the one-shot — plus what the static mode shares with them.

Everything under "Settled" is an owner ruling and is not reopened here.
Everything under "Proposals" is a recommendation with alternatives, for the
owner to rule on.

---

## 1. Settled

### 1.1 Four creation modes

Owner ruling, 2026-09-12 (`docs/REALTEST-JUDGEMENT-CALLS.md`, "Owner ruling:
workspace creation modes").

| mode | asks the user | repository | name | prompt |
| --- | --- | --- | --- | --- |
| dynamic one-shot | a prompt | current workspace's repository, off its default branch | daemon mints | required |
| dynamic normal | a prompt | same | daemon mints | optional |
| dynamic fork | a prompt | same | daemon mints | required in practice; the fork's parent is the current workspace |
| static | repository and name | picked | supplied | none |
| static fork | a name | current workspace's repository | supplied | none; the fork's parent is the current workspace |

The child variants are their own commands, not a prefix argument (owner
ruling, 2026-09-12; `lisp/verbs.el` `agent-repl-create-child-workspace`,
`agent-repl-create-child-workspace-static`, bound `SPC TAB c` / `SPC TAB C`).

The static fork is the named counterpart of the dynamic fork
(`agent-repl-fork-workspace-static`, bound `SPC TAB F`, beside the dynamic
fork's `SPC TAB f`): it asks a required name and no prompt, so the fork comes
up idle on the forked conversation. Both fork commands share
`agent-repl-verbs--create-fork`. The daemon accepts a fork carrying a name and
no prompt: `CreateWorkspaceStandard.name` and `CreateWorkspaceParent.fork` are
independent in the proto, `validateCreateWorkspaceRequest` constrains neither,
and `branchFor` takes a supplied name before it ever consults the prompt.

The Emacs side already implements this shape:
`agent-repl-verbs--dynamic-repository` (`lisp/verbs.el`) reads the repository
off the current workspace's roster section and refuses without one;
`agent-repl-verbs--create-standard` asks the prompt for `dynamic` and the
repository plus a required name for `static`; the dynamic path sends no
`base_ref` at all, and that absence IS "the repository's default branch"
(`daemon/internal/workspace/create.go`: `baseRef == "" → Git.DefaultBranch`).

### 1.2 A bring-up failure is footer-only

Owner ruling, 2026-09-12. Landed as `FooterStatusActivityStartFailed`. Nothing
in this design adds a feed row for a failed create.

### 1.3 The one-shot policy is the repository's

Owner ruling, 2026-09-12. Implemented on branch `policy/repo-oneshot`, held
unmerged pending this design.

- A repository states its policy in `<repo main checkout root>/.agent-repl/prompts/`
  (`prompts.PolicyDirRel`, `daemon/internal/prompts/policy.go`).
- Doom's policy is the module corpus, and only doom's:
  `prompts.IsModuleRepository(repoRoot, checkoutRoot)` answers true when the
  daemon's own module checkout is the repository root or a directory inside
  it, which is exactly "the doom test is a module checkout inside the
  repository root".
- The corpus is NEVER a fallback for another repository.
- A missing policy is detected by the DAEMON, in `requireOneShotPolicy`
  (`daemon/internal/workspace/oneshot.go` on the branch), BEFORE the id is
  minted, the creation job recorded, or git touched. It is returned as the
  `one_shot_policy_missing` create refusal
  (`CreateWorkspaceOneShotPolicyMissing`, field 12 of `CreateWorkspaceError`),
  carrying `repository_root`, `policy_dir` and `missing_files`.
- Emacs warns in the minibuffer and never detects the absence itself.

### 1.4 Naming is a headless Haiku call inside `Create`

Every dynamically created workspace whose client supplied no name gets its
name from a headless Haiku call the daemon makes inside `Create`, constrained
to at most three words, lowercase, hyphenated.

- The current word-truncation path — `workspace.Slug`
  (`daemon/internal/workspace/naming.go`) — is **DELETED**, not kept as a
  fallback.
- A failed naming call is a create refusal arm surfaced to the caller.
- This applies identically to the Emacs wire path and the on-disk command-file
  path. Both funnel into `verbs.Create`
  (`daemon/internal/commandfile/ingress.go` `applyCreate`), so there is one
  naming site, not two.

### 1.5 The naming call is in-process, not on the dispatch JSON channel

The daemon calls Haiku for the name only, with structured output, and
continues in-process. It does NOT route through the dispatch JSON channel;
that channel stays for genuinely out-of-band callers. The reasons, stated:

- `CreateWorkspace` answers Emacs **synchronously**, and Emacs selects the
  result: `agent-repl-verb-create`'s `:select` calls
  `agent-repl-verbs--select-created` on `CreateWorkspaceSuccess.workspace`,
  which is documented as the one place the minted identity exists before the
  roster push carries it (`lisp/verbs.el`,
  `agent-repl-verbs-select-minted`). A create that detoured through a file
  channel would have no answer to select on.
- Refusals must reach the caller. The whole refusal vocabulary of
  `CreateWorkspaceError` exists because the rpc answers the client; the
  command-file route has no caller and quarantines instead
  (`ingress.go` `ApplyFile`: "a file has no caller to answer").
- A model writing JSON adds a failure surface. The dispatch channel's contract
  is a hand-authored array validated by `Entry.Validate`
  (`daemon/internal/commandfile/entry.go`); asking a model to emit it puts a
  parse failure between the user's keystroke and the workspace.

---

## 2. What `Create` does today, in order

`verbs.Create` (`daemon/internal/workspace/create.go`) is the single funnel.
Its order is stated in its own doc comment and is where every change below
lands.

| step | site | note |
| --- | --- | --- |
| 1 | `validateCreate` | a one-shot needs a prompt; an ungated permission mode needs consent |
| 1b (branch) | `requireOneShotPolicy` | one-shot only; refuses `one_shot_policy_missing` before anything is minted |
| 2 | `wsm.NewWorkspaceID()` | the creation job's key, and today the name of a promptless unnamed create |
| 3 | `branchFor` | supplied name → `Name(Prefix(), supplied)`; no prompt → `Name(Prefix(), "workspace-"+id)`; else `Slug(prompt)` |
| 4 | `WorktreeDir` | bare branch component under `<repo>-worktrees/` (`naming.go`) |
| 5 | `Git.DefaultBranch`, `Git.ResolveRef` | an unresolvable base ref is `base_ref_unresolved`, with the ref in the arm |
| 6 | `mergeTargetDir` | parent's worktree for a child, else the repo main worktree |
| 7 | `DB.PutCreationJob` | merge geometry, actions, base ref, one-shot, initial prompt, consent — recorded BEFORE materialization |
| 8 | `Git.CreateWorktree` | `git worktree add -b <branch> <dir> <baseRef>` (`gitclient.go`) |
| 9 | `Register` | mints the registry id; the job is re-keyed onto it |
| 10 | `PutSession` (+ `forkTranscript`) | model, permission mode, host session id; a fork ports the parent transcript under a FRESH vendor session id |
| 11 | `Sessions.Start` | bring-up |
| 12 | `submitInitialPrompt` | one-shot decoration, then `Queue.Submit` with origin `WORKSPACE_CREATED` |

Two facts this design leans on:

- **Naming happens at step 3**, before any filesystem or git work. A naming
  call inserted there costs nothing already spent.
- **The name IS the branch IS the worktree directory component.**
  `branchFor` answers one string used for all three, plus `RegisterFacts.Name`.

---

## 3. Proposal A — how the daemon makes a headless model call

### What exists today

| facility | what it is | usable for naming? |
| --- | --- | --- |
| `daemon/internal/classifier/vendor.go` | the daemon's OWN headless vendor run: `exec.CommandContext(ctx, bin, "-p", "--output-format", "text")` with the composed question on **stdin** so it never rides an argv | **yes — this is the precedent** |
| `daemon/internal/login/manager.go` | `exec.Command(m.vendorBin, "/login")` on a pty, env `CLAUDE_CONFIG_DIR=<configDir>` (`childEnv`) | no; interactive |
| `agent-shim/claude/shim/` | the per-session TypeScript shim driving the vendor SDK | no; one process per workspace session, brought up by `Sessions.Start` — a create has no session yet at step 3 |
| `agent-shim/claude/shim-sidecar/` | reads the vendor's on-disk transcripts (`boottime_*.go`, `cycle.go`) | no; it makes no vendor calls at all |
| `daemon/internal/account/` | `ConfigDirFor(dir)` routes a workspace path to its config root, `Read` reports the signed-in email, `FindTranscript`/`PortTranscript` locate and copy transcripts | supplies the account, not the call |

Note a live gap: `buildJudge` (`daemon/cmd/claude-repld/graph.go:1000`) calls
`classifier.New(guard, "", promptsDir)` with an **empty** vendor binary, and
`vendorJudge.Judge` refuses on `j.bin == ""`. In production today the
classifier's headless run cannot fire. Whatever the naming call uses must
resolve its binary properly rather than inherit that hole.

### Recommendation: A1 — a shared `internal/headless` package around `claude -p`

One package, extracted from the classifier's `runVendor`, used by both.

```
headless.Run(ctx, Request{
    Bin        string          // resolved: AGENT_REPL_CLAUDE_BIN, else "claude"
    Model      string          // "haiku"
    ConfigDir  string          // account.Resolver.ConfigDirFor(repoRoot)
    Prompt     string          // on stdin, never argv
    Timeout    time.Duration
}) (Response{ Text string; Model string; Duration time.Duration }, error)
```

- argv: `<bin> -p --model haiku --output-format json`.
- every headless run also carries the call-owned pin `headless.pinnedArgs`:
  `--settings '{"alwaysThinkingEnabled":false}' --strict-mcp-config
  --safe-mode`, so the user's interactive settings (extended thinking, MCP
  connectors, plugins, hooks) never change a daemon question's behavior.
  `--bare` is unusable because it refuses OAuth.
- stdin carries the prompt, exactly as the classifier does, so a process
  listing never shows the user's words.
- env: `os.Environ()` plus `CLAUDE_CONFIG_DIR=<config dir>`, the same shape
  `login.childEnv` uses, so the naming call bills the account the workspace
  will run under.
- `envc.VendorGuard.Check(site)` before the spawn, with the site name
  `"workspace_naming"` beside the existing `"classifier"` and `"login"`.
- `--output-format json` gives an envelope with a `result` string plus usage
  and cost; the model's own answer is that string. The daemon parses the
  envelope, reads `result`, and then applies its OWN validator (§4.4). The
  CLI has no schema-enforcement flag, so "structured output" here means a
  parsed envelope plus a hard daemon-side validator, never trust in the
  model's formatting.

Why A1: it is the only path that already exists in the daemon, it inherits the
guard, the fake-binary substitution and the stdin discipline, and folding the
classifier into it removes the second copy of an exec site rather than adding
one.

### Alternatives

| option | shape | why not recommended |
| --- | --- | --- |
| A2 — call the Anthropic HTTP API directly from Go | an SDK/HTTP client in the daemon | a second credential path. The daemon has no api key plumbing; the account plumbing it does have is CLI config roots (`account.Roots`, `.claude.json`). Adding key handling for a three-word name is the largest change for the smallest feature. |
| A3 — route through the session shim | ask the workspace's own shim | no session exists at step 3, and the naming answer must precede the worktree the session runs in. Circular. |
| A4 — a naming call on the shim-claude-sidecar | reuse a resident service | it is a transcript reader with no vendor client and no account routing; this would build A2 inside a service that has no reason to hold it. |
| A5 — `--output-format stream-json` | streaming envelope | nothing consumes partial output for a three-word answer; it only adds a parser. |

### The mocking story

Repo rule: every vendor call is mocked in every test; the fake SDK is used
everywhere; `AGENT_REPL_FORBID_VENDOR_CALLS` makes every vendor entry point
refuse loudly (module `AGENTS.md`, "No real Claude/Anthropic calls from
tests").

| layer | how the naming call is faked |
| --- | --- |
| `internal/workspace` unit tests | a `namerFunc` seam on `verbs`, exactly the `loaderFunc`/`splicerFunc`/`runnerFunc` seams `classifier` already uses. No exec, no filesystem. |
| `internal/headless` unit tests | a scripted `runnerFunc`; one test per outcome — good answer, bad answer, non-zero exit, timeout, guard refusal. |
| daemon integration (`daemon/integration/harness`) | the harness already writes a fake `claude` and exports `AGENT_REPL_CLAUDE_BIN` (`harness/daemon.go:521`, `harness/fakes.go` `fakeClaudeScript`). The fake script gains a branch that answers a naming prompt with a canned three-word slug. |
| `-fake` daemon mode | `contracts.Fake()` already swaps the classifier for `classifier.NewFake()`. A `naming.NewFake()` answers a deterministic slug derived from the prompt, so a fake-mode stack still creates workspaces with readable names. |
| the guard | with `AGENT_REPL_FORBID_VENDOR_CALLS` set and no fake, the call refuses at `guard.Check("workspace_naming")` and the create returns the naming refusal arm. That is a first-class covered case, not an accident. |

### The log lines

Levels follow the module's "an invisible action is a logging defect" rule:
routine actions at INFO, per-item detail at DEBUG, and never WARN for a
routine event.

| level | operation | message | context |
| --- | --- | --- | --- |
| INFO | `daemon.workspace.naming_call` | `issued the workspace naming call` | `model`, `repo_dir`, `config_dir`, `attempt` |
| INFO | `daemon.workspace.naming_call` | `the workspace naming call answered` | `model`, `duration_ms`, `name`, `attempt` |
| DEBUG | `daemon.workspace.naming_call` | `the naming prompt` | `prompt` (the composed brief, verbatim) |
| DEBUG | `daemon.workspace.naming_call` | `the naming answer did not validate` | `answer`, `reason`, `attempt` |
| ERROR | `daemon.workspace.naming_call` | `the workspace naming call failed` | `model`, `cause`, `attempts` — paired with the refusal, one record, not two |

---

## 4. Proposal B — the naming brief

### 4.1 Recommendation: a NEW corpus brief, and retire the two old ones

`prompts/workspace-generation-name-prefixed.md` and
`workspace-generation-name-unprefixed.md` are fragments, not briefs: each is
one sentence whose header declares `used by: worktree.el`, and both instruct
a model to fill a `name` FIELD of a dispatch JSON object. Neither is a
standalone question, and the Emacs call site they name is gone.

Recommend one new brief, `prompts/workspace-naming.md`:

- header `<!-- used by: daemon/internal/workspace (Create, the naming call); placeholders: {{prompt}} -->`
- body states the rule the daemon then enforces: lowercase, hyphen-separated,
  at most three words, naming the SUBJECT of the work and never its
  provenance, no prefix, no trailing hash, and nothing but the name in the
  answer.
- the provenance rules are worth carrying over verbatim from
  `~/.claude/skills/create-or-update-workspace/create.md` step 4, which is the
  only place they are written down: never a chat-channel name, never a
  person, never a timestamp or thread id.
- the brief is read at use time through `prompts.Load` + `Splice`, like every
  other brief, so an edit takes effect on the next create with no bounce
  (`daemon/internal/prompts/api.go`).

**The prefix is NOT in the brief.** The old pair existed only to splice
`{{prefix}}` into the model's instructions. The daemon already owns that:
`Name(Prefix(), slug)` composes `<prefix>/<slug>` from `AGENT_WORKSPACE_PREFIX`
(or the legacy `CLAUDE_WORKSPACE_PREFIX`), and `BareName` strips it back for
the worktree directory (`naming.go`). Asking the model to emit the prefix
gives it a way to get the prefix wrong; asking it for the bare slug cannot.
So: the model answers a bare slug, the daemon prefixes.

Alternative B2: reuse the two existing files, one chosen by whether a prefix
is set. Rejected on the same ground — it keeps two files that differ only in a
fact the daemon does not need the model to know.

Alternative B3: put the brief in the repository's `.agent-repl/prompts/`
alongside the one-shot policy. Rejected as a default: naming is a property of
this system's branch conventions, not of the repository's definition of
"done". It could be added later as an OPTIONAL per-repository override without
changing the corpus default. **This is an open question (Q3).**

### 4.2 Where the prefix applies

Unchanged: `Prefix()` reads `AGENT_WORKSPACE_PREFIX` first, then
`CLAUDE_WORKSPACE_PREFIX`. `Name(prefix, slug)` yields `<prefix>/<slug>` for
the branch and workspace name; `WorktreeDir` uses `BareName` so the directory
is the bare slug. A user-SUPPLIED name containing a `/` is taken as it stands
and is not prefixed (`branchFor`) — that stays.

### 4.3 Collision handling

**Today there is none.** `branchFor` derives a name and `Git.CreateWorktree`
runs `git worktree add -b <branch> …`, which fails if the branch exists; the
create then returns `worktree_creation_failed` carrying git's own account. No
code in `internal/workspace` probes for an existing branch or workspace. The
old Emacs-side flow avoided the problem entirely by appending a random
three-letter suffix to EVERY name
(`~/.claude/skills/create-or-update-workspace/run.sh`:
`suffix = "".join(secrets.choice(string.ascii_lowercase) for _ in range(3))`),
so the model's slug was never the whole name.

Recommendation **C1 — the daemon disambiguates deterministically**: after the
model answers and the answer validates, probe for a collision (an existing
branch, or a registered workspace of that name) and, on a hit, append `-2`,
`-3`, … until free. Log the collision at DEBUG with both names. Reasons: the
old system always disambiguated, a three-word slug space collides often for a
repeated kind of task, and a create refused because a stale branch of the same
name exists is a bad answer to a good request.

| alternative | why not |
| --- | --- |
| C2 — restore the random three-letter suffix on every name | it made every name noisy for the 99% that never collided, and the owner's roster reads these names |
| C3 — refuse the create with a `name_taken` arm | correct-by-construction but hostile: the user typed a prompt, not a name, and has nothing to change |
| C4 — ask the model again for a different name | a second vendor call and a second latency budget to solve a problem arithmetic solves |
| C5 — reuse ("find") the existing workspace | changes what a create MEANS; a create that silently selects someone else's workspace is a different verb |

C1 and C3 both need the probe; only the tail differs. **Open question (Q4).**

### 4.4 Validating the model's answer

The three-word rule is the daemon's, enforced on the answer, not hoped for:

- trim, lowercase-compare: the answer must match `^[a-z0-9]+(-[a-z0-9]+){0,2}$`
  — at most three hyphen-separated lowercase alphanumeric words, no leading or
  trailing hyphen, no slash, no path component.
- length bounded by the existing `SlugMaxLen` (40).
- an answer that fails is NOT trimmed, truncated, or repaired. Repairing is
  what `Slug` did, and `Slug` is deleted.

Retry: recommend **exactly one retry**, with the same brief plus a
daemon-composed correction naming what was wrong with the first answer. One
retry covers the common failure (the model wrapped the name in a sentence)
without doubling the worst-case latency more than once. The attempt count
rides the refusal (§5) and the log lines (§3). Alternatives: zero retries
(simplest, and a one-word failure costs the user a retype of the prompt), or
retry-until-timeout (unbounded latency on a silently misbehaving model —
rejected). **Open question (Q5).**

---

## 5. Proposal C — latency budget, and where it shows

### Is the create already asynchronous from Emacs's view?

**No.** `CreateWorkspace` is a unary rpc and Emacs waits for the answer:
`agent-repl-verb-create` passes an `:on-success` that messages
`agent-repl: workspace requested` and then, for `:select`, calls
`agent-repl-verbs--select-created`. The transport dial itself is a blocking
loopback connect by design (`agent-repl-connect--open-socket`; module
`AGENTS.md`, "ITS CONNECT BLOCKS, AND THAT IS NOT A TUNING CHOICE"), and the
answer is what carries the minted `WorkspaceRef` the selection stands on.

So a naming call inside `Create` is added directly to the user's wait between
pressing `SPC TAB n` and the workspace becoming current. It is not hidden by
any existing asynchrony.

What already fills that window: everything in §2 steps 4–12 — a
`git worktree add`, a registration, a session record, and `Sessions.Start`,
which is a shim bring-up. The naming call is added to a wait that is already
seconds long, not to an instant one.

### Recommendation

| knob | value | basis |
| --- | --- | --- |
| naming call timeout | 15s per attempt, as an explicit `context.WithTimeout` in `headless.Run` | it is a Haiku call over a handful of tokens; a call that has not answered in 15s has failed, and the module's bounds rule is "a small multiple of the observed healthy max" — which must be MEASURED once the call exists and this number then re-set from it |
| retries | one (§4.4), so the worst case is two timeouts | |
| where the wait shows | the minibuffer, as an explicit `naming the workspace` phase before `workspace requested` | the owner's standing rule that startup phases are echoed as messages, not only in a mode line |
| the footer | nothing new | a create has no workspace yet, so it has no footer; the footer carries the create's outcome only in the bring-up-failure case already landed (§1.2) |

Two things the owner may want to rule on: whether the pre-answer minibuffer
message is wanted at all, and whether the timeout should be a `defcustom`-like
daemon flag rather than a constant. **Open question (Q6).**

---

## 6. Proposal D — the refusal arm for a failed naming call

Proposed, not decided. The shape follows the arms beside it: a message with
fields read off the failure, added to `CreateWorkspaceError.cause`, with a
matching `workspace.Arm…` constant and a `refuseWith` site.

```proto
// The daemon could not mint a name for a create that supplied none.
message CreateWorkspaceNamingFailed {
  // The model the naming call asked.
  string model = 1;
  // What went wrong, as one of a closed set the daemon spells.
  CreateWorkspaceNamingCause cause = 2;
  // How many attempts were made before giving up.
  uint32 attempts = 3;
  // The last answer the model gave, when there was one. Empty when the
  // call never answered.
  string answer = 4;
}

enum CreateWorkspaceNamingCause {
  CREATE_WORKSPACE_NAMING_CAUSE_UNSPECIFIED = 0;
  // The vendor guard refused the call.
  CREATE_WORKSPACE_NAMING_CAUSE_FORBIDDEN = 1;
  // The call did not answer within its budget.
  CREATE_WORKSPACE_NAMING_CAUSE_TIMEOUT = 2;
  // The call failed: a non-zero exit, an unreadable envelope, no binary.
  CREATE_WORKSPACE_NAMING_CAUSE_CALL_FAILED = 3;
  // Every answer failed the three-word rule.
  CREATE_WORKSPACE_NAMING_CAUSE_INVALID_ANSWER = 4;
}
```

Daemon-side arm name: `ArmNamingFailed = "naming_failed"`, beside
`ArmNoSlug`, `ArmBriefMissing` and `ArmOneShotPolicyMissing`
(`daemon/internal/workspace/refusal.go`).

Questions inside this shape, for the owner:

- Does `ArmNoSlug` survive? With `Slug` deleted, its two current refusal sites
  in `branchFor` and `validateCreate` change meaning: the promptless one-shot
  check ("a one-shot creation must carry the prompt it runs") is really an
  argument-validation failure, and the "no slug can be derived" site
  disappears entirely. Recommend keeping `no_slug` for the promptless one-shot
  and letting `naming_failed` own everything the model could not answer.
- Is `answer` wanted on the wire? It is the most useful field for diagnosing a
  bad brief and the only one that carries model-authored text to a client.
- Should `cause` be an enum or four sibling arms? The arms around it are
  presence-only messages; an enum inside one message is the smaller change but
  the less idiomatic one for this proto's own style.

**Implementers do not decide this.** Per the module's standing rule, a
proto shape is the owner's; this section is the description, not a landing.
**Open question (Q1).**

---

## 7. Proposal E — the command-file path

### Where it stands

`ingress.applyCreate` (`daemon/internal/commandfile/ingress.go`) builds a
`workspace.CreateSpec` from an `Entry` and calls the same `Verbs.Create`. The
`Entry` struct has `one_shot bool` and no finish — which is now the whole
story, since no create form has one.

Naming: `Entry.Validate` accepts a create with `name` OR `prompt`, so a
nameless entry already reaches `branchFor` and is named by `Slug` today.
Under §1.4 that same entry is named by the Haiku call — the file route
inherits it for free, because it inherits `Create`.

### RULED, 2026-09-12: there is no finish field, here or anywhere

The proposal was a flat `"finish"` string on `Entry` reusing the
`finishOrigin` spellings. It is MOOT: the finish choice is retired outright, so
the channel has nothing to gain a field for. `applyCreate` now sets `OneShot`
and no more, and what happens on completion is the repository's own directive
exactly as it is for a wire create.

---

## 8. One-shot decoration, end to end, under repository policy

RULED, 2026-09-12: **there is no "open PR" option and no finish choice at all.**
A repository states, in ONE canonical plain-English file, what is to be done on
completion. The daemon concatenates it to the commission behind the literal
sentence

```
when you're all done, please do the following postprocessing directive: <the repository's directive>
```

and the AGENT carries it out. The daemon performs no finish action
programmatically: `OnOneShotTurnConcluded`, the queue's finish hook, the
creation job's finish column, `finishOrigin`/`parseFinishOrigin`,
`createPrCommand` and `openPrFollowup` are all retired, and so are the proto's
`finish` oneof and the two error arms that policed it. Doom's directive is the
module corpus's `oneshot-completion-directive.md`.

The policy source is chosen once and used at every step.

| stage | site | reads |
| --- | --- | --- |
| create, before anything is minted | `requireOneShotPolicy` | probes `policy.Dir` for the briefs `oneShotPolicyBriefs()` names — always exactly `workspace-autonomous-preamble` and `oneshot-completion-directive`. Missing → `one_shot_policy_missing`, nothing built. Records `daemon.workspace.oneshot_policy_source` at INFO with `repository_root`, `source`, `policy_dir`. |
| create, the first prompt | `decorateOneShot(raw, policy)` | `prompts.Wrap(preamble) + the user's words + prompts.Wrap("\n" + completionDirectiveLead + directive)`. The user's own text is the only unwrapped span, so the drawn bubble is their words alone while the agent receives the whole composition. |
| the directive | `oneshot-completion-directive` | plain English, submitted verbatim; it declares no placeholders, and one that declares any fails the create |
| submission | `submitInitialPrompt` | one turn record, then `Queue.Submit`, origin `PROMPT_ORIGIN_WORKSPACE_CREATED` |
| conclusion | — | nothing. The daemon does not act on a one-shot's turn ending. |

### A directive may still name a skill the repository cannot supply

A directive's WORDS are the repository's, but any command those words invoke
resolves out of the agent's skill search path, which is the user's. Doom's
directive names `/create-or-update-workspace`, a user-level skill in
`~/.claude/skills/`. A repository may instead name a repo-local skill
(`.claude/skills/…` in its own tree) or plain `gh` instructions, since the
directive is free text and nothing in the daemon constrains it.

The asymmetry is unchanged and is now the whole of it: the daemon PROBES for
the directive file at create time and refuses when it is absent, but it cannot
probe for whatever the directive names. That is a property of handing the work
to the agent, not a gap to close daemon-side — the daemon no longer runs any
part of the finish.

**What a non-doom repository must provide, minimally:**

| file | required for | must contain |
| --- | --- | --- |
| `.agent-repl/prompts/workspace-autonomous-preamble.md` | every one-shot | the "do not wait for further instructions" preamble; declares no placeholders |
| `.agent-repl/prompts/oneshot-completion-directive.md` | every one-shot | the plain-English completion directive; declares no placeholders |

Every file follows the corpus format: a first-line
`<!-- used by: …; placeholders: … -->` header, then the text, with the
declared placeholders exactly matching those used (`prompts.Load`).

---

## 9. Merge-before and merge-after apply to ALL workspaces of a repository

Stated, already on the branch (`daemon/internal/merge/run.go`):

- `run.policy` is resolved ONCE at admission (`orchestrator.start` →
  `policyFor`), because the directory is a fact about the workspace's
  repository; the briefs inside it are read at use time, so an edit takes
  effect on the next merge.
- `run.actions(configured, brief)` is the rule, used by both `prePrompt` and
  `postPrompt`: **a wire-supplied action wins**; the repository's file fills
  an EMPTY slot only. A create that configured `merge_actions` runs those.
- An ABSENT `merge-before.md` / `merge-after.md` is not an error — it is what
  every repository looked like before this policy existed. A brief that is
  PRESENT and will not load fails the merge rather than being skipped
  (`prePrompt` returns the error; a stated policy that cannot run is a fault).
- Both briefs declare NO placeholders and are submitted verbatim
  (`prompts.OnDisk.Text` errors on a policy brief that declares any).
- `prePrompt` failures fail the run; `postPrompt` failures never do and ride
  the terminal status.

---

## 10. Implementation plan

Ordered, small, each landable alone. Two are already written.

| # | change | status |
| --- | --- | --- |
| 1 | **Repository one-shot and merge policy.** `prompts.Source`/`Files`, `requireOneShotPolicy`, the merge `actions` rule, the `one_shot_policy_missing` arm, the Emacs and webapp refusal wording, `docs/ONE-SHOT-POLICY.md`. | **written**, branch `policy/repo-oneshot`, 9 commits, unmerged pending this design |
| 2 | **Four creation modes and the child commands.** `agent-repl-verbs--dynamic-repository`, `--create-standard`, `SPC TAB n/N/c/C`. | **landed on master** |
| 3 | `internal/headless`: the shared guarded exec, extracted from `classifier.runVendor`, with the classifier moved onto it and its empty-binary hole (`graph.go:1000`) closed. Pure refactor plus one bug fix; no naming yet. | **landed** (`create/headless-naming`) |
| 4 | The naming brief, and retirement of the two `workspace-generation-name-*.md` fragments. Docs and corpus only. Landed as `prompts/workspace-name-from-prompt.md`, which declares `{{prompt}}` and `{{correction}}` — the retry's correction is spliced into the corpus's own words rather than appended by the daemon. | **landed** (`create/headless-naming`) |
| 5 | The proto arm for a failed naming call (§6), landed as owner-settled text. Proto only, plus regenerated bindings. Owner settled Q1: ONE message, `cause` a STRING and not an enum, `answer` on the wire, `no_slug` kept for the promptless one-shot. | **landed** (`create/headless-naming`) |
| 6 | `Create` names through the call: the seam landed as `Deps.Headless` (a `headless.Runner`) rather than a `namerFunc`, so the naming logic is the workspace package's and only the exec is shared; `branchFor` rewritten, **`Slug` and `words` deleted**, the validator, the retry, the collision probe (through a new `gitclient.Git.BranchExists`, so an absent branch is an ANSWER and not an error record), the refusal site, the log lines. | **landed** (`create/headless-naming`) |
| 7 | Emacs: the minibuffer phase message and the naming-refusal warning, beside the `one_shot_policy_missing` warning landing in 1. The webapp's exhaustive refusal union gained the arm with it. | **landed** (`create/headless-naming`) |
| 8 | The command-file `finish` field (§7): `Entry`, `Validate`, `applyCreate`. | **withdrawn** — the finish choice is retired (owner ruling, 2026-09-12) |

Test obligations, per the module's one-suite-per-source rule: `internal/headless`
gets its own suite; `internal/workspace` gains naming cases (valid answer,
invalid answer, retry succeeds, retry fails, guard refusal, timeout,
collision, supplied name skips the call entirely, static mode skips it);
the integration harness's
fake `claude` gains its naming branch; the Emacs suites gain the refusal
message sites.

---

## 11. Open questions for the owner

| # | question | recommendation |
| --- | --- | --- |
| Q1 | The naming-failure refusal arm's shape (§6): one message with a cause enum, or four sibling arms? Does `answer` ride the wire? Does `no_slug` survive? | one message with a cause enum, `answer` included, `no_slug` kept for the promptless one-shot only |
| Q2 | Does the command-file channel gain a `finish` field (§7)? | **RULED, 2026-09-12: no.** There is no finish choice at all; completion is the repository's own directive |
| Q3 | Is the naming brief corpus-only, or may a repository override it in `.agent-repl/prompts/` (§4.1)? | corpus-only for now; a per-repository override is a later, additive change |
| Q4 | Collision handling (§4.3): deterministic `-2`/`-3` suffix, or refuse with a `name_taken` arm? | the deterministic suffix |
| Q5 | Retry policy for an invalid naming answer (§4.4): one retry, or none? | exactly one |
| Q6 | Is the pre-answer `naming the workspace` minibuffer message wanted, and should the timeout be a daemon flag rather than a constant (§5)? | yes to the message; a constant until a measurement justifies a flag |
| Q7 | The open-pr finish names user-level skills a repository cannot supply (§8). Should the PR command become a required policy brief the daemon splices, instead of the hardcoded `CreatePrSkill`? | **RULED, 2026-09-12: moot.** The whole finish is now the repository's own plain-English directive, which names whatever it likes |
