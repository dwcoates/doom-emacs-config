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

## Running it (the project lead)

```
cd modules/app/agent-repl/agent-shim/claude/shim
unset AGENT_REPL_FORBID_VENDOR_CALLS
node scripts/capture/capture.mjs --i-am-the-project-lead-capture-run
```

Useful flags: `--only <name[,name…]>` to capture a subset, `--out <dir>` to
write somewhere other than `scripts/capture/captures/`, `--prompts <file>` for
an alternate corpus.

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
- `persistSession`, a per-scenario `cwd`, and an `AbortController`.

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
captures/<scenario>/meta.json      prompt, expectations, controls driven,
                                   unparsed lines, and errors
captures/SKIPPED.json              the manual scenarios, and why
```

Everything is passed through `anonymize.mjs` before it lands — the walker
`testdata/corpus/MANIFEST.md` specifies: credential-shaped values become
`REDACTED`, strings over 900 chars are truncated to 400, declared opaque blobs
(`signature`, base64 image `data`) are truncated to a prefix, and every
structural field — uuids, session ids, paths, timestamps, tool-use ids, object
keys — survives verbatim. A capture directory is safe to commit.

## The corpus

`prompts.json` holds one entry per item in the SHIM directive's coverage list.
`prompt` is `null` exactly when the scenario has **no prompt-only
provocation** — a 429, a query death, a cold resume past the cache TTL, a
refusal, a max-tokens-on-schema failure — and `manual` then states the operator
procedure. `manual_setup` marks an otherwise prompt-driven scenario that needs
a step first (a git repo, a pre-existing multi-turn session, a bulk file tree).
`--list` prints which is which.

## Afterwards

1. Review each `stream.jsonl` against its `meta.json` `expect` list; anything
   expected and absent is a finding, not a silent pass.
2. Land the captures as the converter suites' goldens.
3. **Rebuild `src/fake/scenarios/*.ts` from the captures**, not from the
   declarations — that is the ordering the whole exercise buys.
4. Fold any newly observed shape into `testdata/corpus/` with a MANIFEST row,
   per that directory's contract (a shape gap becomes a fixture FIRST, then a
   fix).
