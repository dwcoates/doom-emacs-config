# Adopting a workspace as a child

- **ADOPT.** `workspace set WS -parent here` makes an existing workspace
  this workspace's child. Its conversation is untouched. It nests under this
  workspace in the sidebar, and this workspace owns it, follows it and is
  told of it as an owner is. A retarget (`-parent` or `-top-level`) that
  names no `-lands-into` KEEPS where WS lands now: its branch was cut from
  there, and moving it would merge it into a branch it was never cut from.
  The relationship's landing default applies only at `workspace create`.
  `-lands-into WS|main` changes the landing, alone (the relationship is
  kept) or with `-parent`/`-top-level`. To also land an adopted workspace
  into this one, add `-lands-into here`, ONLY when its branch was cut from
  this workspace's; otherwise keep its landing. `-top-level` makes it a
  top-level peer.
