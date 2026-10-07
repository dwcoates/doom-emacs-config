# The capture harness

The one-time supervised REAL capture the project lead dispatches — the single
sanctioned exception to this repository's no-real-vendor-calls rule.

## Why it exists

`docs/overhaul/shim.md` (deferred vetting item 5) rules that the golden
transcripts the SDK→`conversation.v1` converter is graded against must be REAL
captures from the actual agent binary, and that **the mocked vendor's scenario
scripts are rebuilt FROM these captures**. That ordering is the whole point: if
the mock and the converter were both written from our reading of the vendor,
they would agree with each other and be wrong together. A capture the mock is
derived from cannot agree with us by construction.

## It has NOT been run

Nothing under `captures/` exists yet, and this wave does not create it. Until
the lead dispatches the capture run, the repository's goldens are:

- the existing real fixtures in `modules/app/agent-repl/testdata/corpus/`
  (harvested 2026-07-23, documented in that directory's `MANIFEST.md`), and
- the pinned SDK's own declarations, guarded by `test/sdk-canary.test.ts`.

## Refusing to run is the default

`capture.mjs` makes real API calls against the operator's account. It runs only
when BOTH hold:

1. `AGENT_REPL_FORBID_VENDOR_CALLS` is unset or empty — every test harness in
   this repo and every shim implementation agent's shell sets it, on purpose;
2. `--i-am-the-project-lead-capture-run` is passed on the command line.

Anything else exits **2** with an explanation and never loads the SDK. Two
inspection modes need neither key and call nothing:

```
node scripts/capture/capture.mjs --list     # every scenario, and which are manual
node scripts/capture/capture.mjs --check    # validate prompts.json, count the corpus
```

## Authentication — pick exactly one mechanism

The first real run captured nothing: every scenario got a fresh EMPTY
`CLAUDE_CONFIG_DIR`, so the vendor started logged out and answered
`Not logged in · Please run /login` in 33 ms at zero cost. Isolation and
authentication were in conflict and nothing had chosen between them.

**The scratch working directory stays scratch in every mode.** Only the account
root varies, and exactly one of these three supplies it:

| Mechanism | What it does | When to use it |
|---|---|---|
| `--config-root <dir>` | Uses an existing account root as the vendor's account root: named through `CLAUDE_CONFIG_DIR` for any other directory, but for the default `~/.claude` the variable is left UNSET, because the CLI keys its Keychain entry on that variable and an explicitly named default root reads as a different, logged-out account. | **Preferred, and production-faithful** — the real shim runs against the real root, so these captures carry the settings, hooks and CLAUDE.md a real session has, and the vendor emits the `permission_denied` messages the gate relies on. |
| `--seed-credentials` | Keeps a fresh scratch root and copies `.credentials.json` plus the account/onboarding fields of `.claude.json` into it. | Maximum isolation. The captured session has none of the operator's settings, so no policy denials and no CLAUDE.md. |
| inherited token | `CLAUDE_CODE_OAUTH_TOKEN` or `ANTHROPIC_API_KEY` already exported; passed through to the SDK. A fresh scratch root then needs no credential file. | CI, or an API-key account. |

`--credentials-from <dir>` overrides where `--seed-credentials` reads from
(default `~/.claude`).

**Exactly one must be in effect.** Two is ambiguous — the capture could not say
which credential produced it — and zero is the failure above. Both are refused
**before the first scenario runs**, by a preflight that verifies the credential
source actually exists; discovering this after a 75-scenario run is how the
first attempt was wasted.

> **On macOS, `--seed-credentials` may not work.** The vendor keeps OAuth
> credentials in the system **Keychain** and leaves `~/.claude/.credentials.json`
> as a **zero-byte file** (that is its state on the machine this was written on).
> Seeding it copies no credential. The preflight detects the empty file and
> refuses by name, pointing at `--config-root` instead.

### What `--config-root` does to the operator's account

The vendor writes each scenario's transcript into that real root, under
`projects/<cwd-slug>/`. After the scenario the harness copies **only that one
slug** into the capture and then **deletes it from the root**, leaving the
account as found. The slug encodes the scenario's own `mkdtemp` cwd, so it
cannot collide with any real project of the operator's. Nothing else in the root
is read, copied, or touched.

Under `--seed-credentials` the credential and `.claude.json` are **wiped from
the scratch root before it is copied into the capture**, so a credential can
never reach a committed fixture. Only allowlisted account fields are seeded in
the first place — never the operator's project history, MCP connections, or
telemetry.

### A capture never reaches the live agent-repl

Under `--config-root` the vendor runs the operator's user-level skills and
hooks. On 2026-09-01 the worktree scenario's model reached for the
`create-or-update-workspace` skill, whose `run.sh` wrote a `create` dispatch
into the LIVE `~/.claude-emacs/output/`; the running daemon's command-file
ingress applied it and registered the scratch repository and a
`ABC/first-kept-fdp` workspace in the owner's `wsm.db`.

- Every world carries its own `agent-repl-state/` root, and the vendor child's
  `AGENT_REPL_STATE_DIR` and `CLAUDE_REPL_STATE_DIR` name it, overriding any
  inherited value (`childEnvFor`). Every in-repo producer honors
  `AGENT_REPL_STATE_DIR` first.
- That workspace skill lives outside this repository and prefers
  `~/.claude-emacs` over either variable whenever the directory exists, so the
  harness also runs a tripwire after each scenario: any command file in the
  live ingress (pending, claimed, applied or quarantined) that names the
  world's scratch quarantines the scenario with an `isolation` error
  (`liveDispatchesNaming`, read-only).

## Running it (the project lead)

```
cd modules/app/agent-repl/agent-shim/claude/shim
unset AGENT_REPL_FORBID_VENDOR_CALLS
node scripts/capture/capture.mjs --i-am-the-project-lead-capture-run \
  --config-root ~/.claude
```

Useful flags: `--only <name[,name…]>` to capture a subset, `--out <dir>` to
write somewhere other than `scripts/capture/captures/`, `--prompts <file>` for
an alternate corpus.

## A capture must EARN the right to be a golden

The first run also wrote an authentication failure into `captures/` as a
finished fixture, exiting 0 with `errors: []`. The detail that made it look fine
was that `result.subtype` was **`"success"`** — the vendor spells an
authentication failure as a success-subtype result carrying `is_error: true`,
so the obvious check passes on that exact transcript.

Every scenario is now staged under `captures/_inflight/<name>/` and moved to
`captures/<name>/` only after it is classified. A capture is **quarantined**
under `captures/_failed/<name>/` when its result carries `is_error`,
`terminal_reason: "api_error"` or a non-null `api_error_status`, when any
message carries `is_api_error_message`, when the turn never reached a terminal,
when the SDK produced no messages, or when the run threw. The evidence is kept —
you need to read the failing transcript — it just cannot sit where the converter
suites will read it as truth. The run then exits **3**.

Only `api-error-classes` is exempt (corpus key `expects_api_error`), because an
API error is precisely its golden.

`apiKeySource` is deliberately **not** a failure signal: a subscription session
legitimately reports `"none"`. It is recorded in `meta.json` for you instead.

Every scenario runs in its **own scratch world**: a fresh temp directory
holding the working directory (`cwd_setup`'s files are materialized there), a
fresh `CLAUDE_CONFIG_DIR`, and a fresh spool root. Nothing touches the
operator's real `~/.claude`, and every file the vendor writes during the run is
inside the capture.

The options are the **shim's production options**, deliberately — a capture
taken under different options is a golden for something nobody ships:

- the `claude_code` preset system prompt with the canonical metaprompt appended
  (the preset carries the environment block the model needs to resolve `~`);
- `settingSources: ["user", "project", "local"]` (without them the vendor never
  emits the `permission_denied` messages the permission gate relies on);
- `includePartialMessages` (the entire streamed-prose plane);
- `forwardSubagentText` (without it a subagent's prose never arrives at all);
- `persistSession`, a per-scenario `cwd`, and a **per-open `AbortController`**
  (never one shared across opens — see the resume note below).

## What it writes

```
captures/<scenario>/stream.jsonl   every SDK message and control exchange,
                                   one line each: {t_ms, dir, msg}
                                   dir is "sdk" for a message off the query's
                                   async iterator, "control" for a canUseTool
                                   request/response and every control verb
captures/<scenario>/files/projects/  the scratch config root's projects/ tree:
                                   the session transcript, the subagent
                                   sidechains, and their agent-<id>.meta.json
captures/<scenario>/files/spool/     the task spools the sidecar tails
captures/<scenario>/meta.json      ok, failure_reasons, prompt, expectations,
                                   auth_mode, api_key_source, controls driven,
                                   unparsed lines, and errors
captures/_failed/<scenario>/       a capture that did not earn golden standing
captures/_inflight/<scenario>/     a run interrupted mid-scenario
captures/SKIPPED.json              the manual scenarios, and why
```

Everything is passed through `anonymize.mjs` before it lands — the walker
`testdata/corpus/MANIFEST.md` specifies: the personal values of whoever ran
it are scrubbed first, in keys and file names too (the home directory becomes
`${HOME}`, which a reader replaying the recording expands back with
`expandHome`; account emails become `personN@example.com`, and the login name, the git
user name's words and anything in `$CAPTURE_PERSONAL_NAMES` become `Someone`;
extra emails go in `$CAPTURE_PERSONAL_EMAILS`); credential-shaped values become
`REDACTED`, strings over 900 chars are truncated to 400, declared opaque blobs
(`signature`, base64 image `data`) are truncated to a prefix, and every
structural field — uuids, session ids, paths, timestamps, tool-use ids, object
keys — survives verbatim. A capture directory is safe to commit.

## The MCP probe server

`mcp-echo.mjs` is a real, minimal MCP server over stdio — line-delimited
JSON-RPC 2.0, **node built-ins only**. It offers `echo` (returns its input) and
`slow` (sleeps, to provoke the vendor's per-call progress heartbeat).

It exists because `mcp-unmodeled-tool` and `mcp-server-healths` previously
shipped a comment-only stub and asked the operator to supply a server at capture
time — so the two scenarios that exist to capture `AgentUnmodeled`, the one arm
producible only by a tool whose schema the shim genuinely cannot know, could not
run at all. Nothing else in the corpus reaches that branch: every other tool is a
modeled built-in.

No dependency was added on purpose: putting `@modelcontextprotocol/sdk` in the
shim's production lockfile so a capture script can echo a string is the wrong
trade. Both scenarios name it through `options.mcpServers`, using the
`{{CAPTURE_DIR}}` token that the harness substitutes with this directory's
absolute path.

## Shared worlds and multi-turn scenarios

Three scenarios used to carry a `manual_setup` note asking the operator to
hand-arrange state the harness could arrange itself. They no longer do.

**A world** is a shared cwd and account root. Scenarios naming the same
`config_root: "shared-world:<name>"` run in **corpus order** against one world,
so a later scenario sees everything the earlier ones did. `prose-streamed`,
`read-whole-head-range`, `grep-content-files-count`, `identity-rotation-clear`
and `compaction-directed` share `shared-world:conversation-history` — which is
how a `/clear` has an identity to rotate and a `/compact` has a conversation to
compact.

**A multi-turn scenario** uses `prompts` (an array) instead of `prompt`. Turns
run on ONE query by default. A turn spelled `{ text, resume: true }` closes the
query and opens a fresh one resuming the same vendor session id — the shim's own
resume path, and the only way to capture what a resume actually costs.

Every open gets a **fresh `AbortController`**, minted by `createQuerySession`.
Closing a query aborts the controller it was opened with, so a controller shared
across opens handed the resumed `query()` an already-fired signal and the
resumed turn died with `Error: Operation aborted` immediately after the
`SessionStart:resume` hook fired. `createQuerySession.open()` tears the previous
query down, aborts only that open's controller, and then mints a new one.

**`cwd_init`** is a shell command run in the scratch cwd before the query. The
worktree scenario uses it to make a real git repository; it previously shipped a
`.capture-init.sh` that was written to disk and **never executed**, so the
worktree tools would have run against an uninitialized directory while the
scenario looked correctly configured. A non-zero exit fails the scenario rather
than warning, because a scenario whose precondition failed captures a golden of
the wrong situation.

## The trigger vocabulary

A scenario's `controls` are scripted verbs, each with a trigger point `at`:

| `at` | Fires when | `after` matches against |
|---|---|---|
| `session_start` | once, right after the query opens | — |
| `on_message` | the first stream message the matcher accepts | an SDK message: `type`, `subtype`, `contains` |
| `on_control` | the first CONTROL RECORD the matcher accepts | a `{kind, tool_name, …}` control entry: `kind`, `tool_name`, `contains` |
| `turn_end` | once, after every turn has finished | — |

`on_control` exists because the permission callback's own records are not stream
messages. A gate the scenario parks is recorded as
`{kind: "can_use_tool_parked", tool_name}` on the `control` plane, and an
`on_message` trigger — however it is spelled — can never observe it, so
`permission-undecidable-parked` waited forever for an interrupt that had nothing
to fire off. It now triggers on `{at: "on_control", after: {kind:
"can_use_tool_parked"}, do: "interrupt"}`. The two planes are strictly separate:
an `on_control` control never sees a stream message, and an `on_message` control
never sees a control record.

## The sweep-end late reclaim

Under `--config-root` the per-scenario reclaim is not the end of the story: the
vendor flushes small late transcript writes into `<config-root>/projects/<slug>/`
*after* that reclaim has already emptied and deleted it, while the SDK child is
still alive — which left the operator's real `~/.claude` dirty.

A SECOND reclaim pass therefore runs once at sweep end, after every query is
closed and every SDK child is gone. For each scenario slug **this run created**
— never any other project directory — it re-copies whatever reappeared into that
scenario's `files/projects/<slug>/`, merging rather than replacing (a destination
file is never overwritten by a SMALLER one, so a truncated late re-write cannot
destroy the captured golden), then deletes the slug directory from the root. Each
late reclaim is logged to stderr:

```
capture.mjs: late reclaim <slug> — N file(s) re-copied, <dir> removed
```

## A provocation the model declines captures nothing

`permission-denied-by-user` used to prompt `rm -rf .`. The model refused that on
its own judgement, so the turn never reached the permission gate and the capture
recorded no `can_use_tool` at all — a golden for denial containing no denial. A
gated scenario's prompt must be a command the model will ACTUALLY attempt
(`rm -f stale.log`, with the file materialized by `cwd_setup`); the refusal is
the capture script's `permission_script` job, never the model's.

The same principle governs `expects_error_subtypes`. A scenario whose golden IS
an error terminal declares it, or the quarantine rule condemns the very capture
it exists for: `permission-undecidable-parked` interrupts a pending gate and the
vendor ends the turn `subtype: "error_during_execution"`,
`terminal_reason: "aborted_tools"`, so that subtype is declared. It is declared
only where the real capture showed it — `hook-cancelled` also drives an
interrupt but terminates `success`/`completed`, and whitelisting an error there
would hide a real failure.

## The corpus

`prompts.json` holds one entry per item in the SHIM directive's coverage list.
`prompt` is `null` exactly when the scenario has **no prompt-only
provocation** — a 429, a query death, a cold resume past the cache TTL, a
refusal, a max-tokens-on-schema failure — and `manual` then states the operator
procedure. `--list` prints which is which, and how many turns each has.

## Afterwards

1. Review each `stream.jsonl` against its `meta.json` `expect` list; anything
   expected and absent is a finding, not a silent pass.
2. Land the captures as the converter suites' goldens.
3. **Rebuild `src/fake/scenarios/*.ts` from the captures**, not from the
   declarations — that is the ordering the whole exercise buys.
4. Fold any newly observed shape into `testdata/corpus/` with a MANIFEST row,
   per that directory's contract (a shape gap becomes a fixture FIRST, then a
   fix).
