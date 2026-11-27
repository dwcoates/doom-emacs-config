# Topbar settings

- **EVERY SETTING THE TOPBAR CAN SET HAS A VERB.** Each verb sends the rpc its
  topbar control sends and the daemon validates it as the control does, so a
  refusal names the same arm the control would be refused by. Run one only
  when the user asked for it, or approved the specific act: each changes what
  a session spends or may do.
  - The per-workspace settings are flags of `workspace set WS`, and WS may be
    this workspace or another, named as every verb names one.
  - The machine's and the repository's settings are verbs of their own.
- **PERMISSION MODE.** `workspace set WS -permission-mode MODE` switches WS's
  session to MODE, the picker's wire spelling.
  - The daemon accepts only a mode the picker serves, and an ungated mode only
    where WS's creation recorded the consent; the refusals are
    `mode_not_served`, `ungated_without_consent`, `no_session` and
    `vendor_refused`.
  - Never ask for an ungated mode to get past a permission prompt.
- **EFFORT.** `workspace set WS -effort low|medium|high|xhigh|max` sets WS's
  reasoning effort. The refusals are `no_session`, `not_supported` (the
  session's model does not take that level) and `vendor_refused`.
- **ACCOUNT.** `workspace set WS -account LOGIN_ROOT` makes WS's session spend
  as the login at LOGIN_ROOT, the `config_dir` of an option the account cell
  serves. The session restarts under it, and a root with no login still
  switches: sign in to it afterwards. The refusal is `unknown_account`.
- **DEFAULT MODEL.** `workspace set WS -default-model M` is the selector row's
  "Make default": the model every NEW session on WS's login starts under, a name
  from the catalog. It never changes WS's own model, which `workspace model`
  sets (`compact-model`). The refusals are `no_session` and `not_in_catalog`.
- **PERSISTENT WIFI.** `runtime persistent-wifi on|off` turns the machine's
  persistent wifi mode (lid-closed operation) on or off. It is the machine's,
  so it names no workspace and changes the topbar of every workspace.
  - The refusals are `power_settings_refused` (the passwordless `pmset` sudo
    grant is absent or refused) and `mode_unreadable`.
  - An agent names the mode it wants, never a toggle.
- **PRODUCTION CHECKS.** `production-mediation pause|resume -repository
  here|DIR` pauses or resumes a repository's production checks, the chip's
  click (`merge-queue` skill, production mediation).
- Already their own verbs: `workspace model` (`compact-model`),
  `-remote-control` (`remote-control`), `-queue-bypass` (`queue-bypass`) and
  `-name` (`rename`).
- Every refusal is printed by its arm name, and the sentence beneath it says
  why. Report it and stop: never work around it.
