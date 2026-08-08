# Automatic prompts

Every file in this directory is a prompt **agent-repl sends to an agent without
you typing it** — a merge-conflict repair brief, a workspace's first message,
the routing classifier's question. They live here, as plain text, so you can
change what the system says without editing source or rebuilding anything.

## The contract

**Edit freely.** Reword, restructure, add constraints, delete paragraphs. The
files are read at the moment a prompt is composed, so an edit takes effect on
the very next use — no rebuild, no daemon restart, no `doom/reload`.

**Keep the placeholders.** `{{like_this}}` is where the system splices in facts
it alone knows: a commit SHA, a worktree path, the failing test output. Every
placeholder a file declares must still appear somewhere in it, and no
placeholder may be invented. Move them, repeat them, reword the sentences around
them — just do not drop or misspell one.

**Keep the header.** The first line of each file is an HTML comment naming the
call site and the placeholders:

```
<!-- used by: <component/file>; placeholders: {{a}}, {{b}} -->
```

It is stripped before the text reaches an agent and exists so you can find the
machinery behind the words. A comment anywhere *other* than the first line is
ordinary content and is sent through as written.

**Whitespace is content.** A file's final newline is treated as the editor's
line terminator and dropped; everything before it is the prompt verbatim,
leading and trailing blank lines included. A prompt that must end with a blank
line therefore carries two newlines at the end.

## When something is wrong

Nothing here fails quietly. There is no baked-in copy of any prompt, so a
missing file, an empty file, or a placeholder that no longer matches its call
site **fails the operation that wanted the prompt**, loudly, through that
runtime's normal error channel:

| Runtime | Where the failure shows up |
| --- | --- |
| Daemon (Go) | The operation returns an error: the merge is left parked or failed, `add-support` answers 500, a classification is reported as ERROR rather than guessing a verdict. All of it lands in the daemon log. |
| Emacs (Elisp) | A plain `error`, so the command that was composing the prompt aborts and the message reaches you. |

A blank prompt would submit an empty turn and a half-substituted one would ship
`{{target_dir}}` to an agent as prose — neither is allowed to happen silently.

## Where the directory is found

| Runtime | Resolution |
| --- | --- |
| Daemon (Go) | Walks up from its own executable to the checkout it was deployed from, then to `modules/app/agent-repl/prompts`. Override with `AGENT_REPL_PROMPTS_DIR` to keep customizations outside the repository. |
| Emacs (Elisp) | `agent-repl-prompts-dir`, resolved next to the module's source at load time. |

## What is *not* here

Not every string the system sends is in this directory. Deliberately excluded:

- **Slash commands** (`/compact`, `/gns-sockets close`, `/create-or-update-pr …`).
  They are commands, not prose, and the CLI parses them.
- **Wire tokens** such as the classifier's `<<QUEUE-JUMP>>` / `<<QUEUE-HOLD>>`.
  The answer is extracted by exact match, so rewording them would break every
  classification. They are substituted *into* the brief instead, so the brief
  around them stays editable.
- **The cache keep-alive ping.** Its text is also a *recognition key*: the
  daemon strips past pings out of the vendor transcript by matching it, so
  editing it would strand every earlier ping.
- **Prompts already exposed as `defcustom`s** (the metaprompt read-directive,
  the explain-config preamble, the prompt-summary brief, the canned diff and
  test commands). They are customizable through Emacs's own mechanism today.
- **`metaprompt.md`**, which already lives as a plain file at the module root
  and is installed by the shim as the session's system prompt.
- **User-typed text**, obviously — only the wrappers around it are here.
