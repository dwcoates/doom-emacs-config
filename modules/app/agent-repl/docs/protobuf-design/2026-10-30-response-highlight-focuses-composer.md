# A response highlight focuses the composer

The owner asked (2026-10-30) that highlighting words of a final response in
the webview move the keyboard straight to the Emacs composer, in insert
state, so the reader never has to click into Emacs to type the revision the
highlight sets up.

## Assumptions (autonomous design)

- The webview already reports every settled highlight to the daemon
  (`agentrepl.v1.SelectResponseText`), and the daemon is the highlight's only
  holder, so the daemon is where the focus request originates. Evidence tier:
  existing code (`daemon/internal/server/response_text.go`).
- Chat threads already do exactly this for their own highlight through
  `agentrepl.v1.WatchHostWorkspaceResponse.chat_text_highlighted`, and Emacs
  handles both reply picks through one handler
  (`lisp/host.el` `agent-repl-host--reply-target-picked`). Evidence tier:
  existing code.

## Iteration sequence

One increment: a new host event arm. Transport, endpoint and response
conventions are unchanged (the host watch stream already exists).

## Landed changes

### `agentrepl.v1.WatchHostWorkspaceResponse.push` gains `response_text_highlighted`

- **What changed.** A new event arm, `response_text_highlighted` (field 18),
  carrying the empty `agentrepl.v1.HostWorkspaceResponseTextHighlighted`.
- **Why.** After highlighting words the reader's next act is typing the
  revision, and the keyboard is still in the webview.
- **Why a new arm and not `chat_text_highlighted`.** That arm names a chat
  thread's highlight. Reusing it for a workspace's final response would make
  one arm mean two facts, and an Emacs reading the log could no longer tell
  which pick moved the keyboard.
- **Why an event and not state.** As for `final_response_clicked`: a topic
  replays to every new subscriber, so a reconnect would steal the keyboard
  again for a highlight made long before.
- **Consequences.**
  - The daemon emits it from `SelectResponseText` only after the highlight
    is taken (not for a cleared selection, nor a refused one).
  - Every re-highlight (a keyboard extension) emits it again. Emacs already
    holds the keyboard after the first, so the repeat re-selects the same
    window and is harmless.
  - The keyboard leaving the webview makes WebKit drop the page's selection.
    The webapp keeps the highlight painted and held regardless
    (`webapp/src/feed/response-highlight.ts`), so the two land together.
