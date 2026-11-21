package harness

// SuccessorOpts is the start of a daemon that SUCCEEDS d on the same machine:
// its state root, the fake shim's profiles, and its default account root. The
// kernel locks come with the state root (LockDirFor), so a successor probes
// the locks its predecessor's shims hold.
//
// THE ACCOUNT ROOT IS CARRIED, never minted afresh: a successor on a fresh
// root finds no transcript and comes up on a FRESH conversation instead of
// resuming its predecessor's. A caller adds what its own successor differs by
// to the returned Opts.
//
// THE STORE IS CARRIED TOO: the real store outlives every daemon, and a
// successor reads the books its predecessor's shims kept (fakestore). So is
// the socket holder: one shared address outlives every daemon.
func (d *Daemon) SuccessorOpts() Opts {
	return Opts{
		StateDir:    d.StateDir,
		ProfileDir:  d.ProfileDir,
		StoreSocket: d.StoreSocket,
		Holder:      d.Holder,
		ExtraArgs:   []string{"--default-config-dir", d.DefaultConfigDir},
	}
}

// adoptConfigDirArgs makes the daemon's account roots the ones its argv
// names. A caller's ExtraArgs may override a root the harness minted (a
// successor carries its predecessor's), and the last flag wins at the
// daemon, so the struct must say the same: SuccessorOpts reads these fields,
// and a successor of a successor would otherwise start on the minted root
// the daemon never used, where no transcript exists.
func (d *Daemon) adoptConfigDirArgs(extra []string) {
	for i := 0; i+1 < len(extra); i++ {
		switch extra[i] {
		case "--default-config-dir":
			d.DefaultConfigDir = extra[i+1]
		case "--multi-repo-config-dir":
			d.MultiRepoConfigDir = extra[i+1]
		}
	}
}
