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
func (d *Daemon) SuccessorOpts() Opts {
	return Opts{
		StateDir:   d.StateDir,
		ProfileDir: d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", d.DefaultConfigDir},
	}
}
