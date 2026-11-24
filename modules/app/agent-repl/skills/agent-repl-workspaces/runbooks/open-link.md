# Opening a link for the user

- `workspace open-link WS URL` opens a link the user must visit, as the
  metaprompt's "Links to visit and commands to run" requires.
- It opens URL in the external browser profile WS's session signs in as (WS is
  `here` or any open workspace): the same launch a click on a link in WS's feed
  makes, routed by the session's account, so the link lands in the work or the
  personal Chrome window that matches.
- Refusals, each printed: an unknown workspace, a workspace ref mismatch, the
  daemon handing over or not yet adopted, `"<url>" is not an absolute url`, no
  external browser configured, or the browser launch failed (with the launch's
  detail).
