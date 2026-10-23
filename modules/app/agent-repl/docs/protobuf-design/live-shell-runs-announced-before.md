# Live shell runs announced before an instant

The sidecar must be able to conclude every detached shell run that was
launched before the machine's boot. After a reboot such a run's spool is gone
(/tmp is emptied) and the sidecar resumes each transcript past the launch it
read before the reboot, so nothing would ever write the run's terminal; the
shim keeps re-announcing it as live, and the daemon's merge of its workspace
waits forever (2026-10-23: three shells held usage-limit-tracking-daemon's
merge).

## Core design principles

- A process cannot outlive the kernel that ran it: a shell announced before the
  machine's boot is ended, whatever the record says. Consequence: the sidecar,
  which owns shell terminals, concludes such runs LOST (swept_up). It does not
  claim anything about runs announced after the boot.

## Landed changes

### store.v1 ShimStore.GetLiveShellRunsAnnouncedBefore (owner pre-approved, 2026-10-23)

- WHAT: a new rpc and its endpoint file. The request names an instant; the
  success arm lists every bash detached_work row with no terminal whose
  announcement the store wrote strictly before it, oldest first, each as the
  existing GetLiveWorkDetached (work + owner) plus the spawning call's activity
  id (origin unit) and the announcement instant.
- WHY: the record is the only place that still names such a run once its
  launch line is behind the sidecar's cursor.
- Unscoped on purpose: the sidecar reads every session, and the boot is shared.
- GetLiveWorkDetached is embedded whole rather than re-declaring work/owner.
- announced_at_ms is the store's own clock, and the upsert never moves it on a
  re-announcement (lifecycle.go), so the comparison is on one host clock.
- Rows with a NULL origin unit or owner are not answered: no terminal can be
  keyed to or written for them; liveWorkGaps already reports them.
- Consequence accepted: the sidecar asks once per process, after its first full
  scan, and waits one more full scan before concluding, so a spool discovery
  finds is concluded from its file (with output) first.
- Verified 2026-10-23 against the live store: the three stuck shells were
  announced at 17:23 and 17:33 against a 17:41:37 boot; five live pre-boot
  shells existed host-wide.
