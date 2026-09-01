package dlog

import "os"

// borrowed is the non-closeable handle on a sink the surfaces own.
//
// It exists because a borrower's ordinary `defer f.Close()` once closed the
// shim log sink's inode out from under every other writer, and the next shim
// spawn inherited a closed fd 3. Close is therefore inert here: the borrower
// cannot take the sink down, and the attempt is recorded so the misuse is
// visible rather than silently tolerated.
type borrowed struct {
	f   *os.File
	log Logger
	// name identifies the sink in the warning.
	name string
}

// File is the underlying descriptor, suitable for a child's fd 3.
func (b *borrowed) File() uintptr { return b.f.Fd() }

// Close is a no-op. The surfaces own the sink's lifetime; only Evict or
// Surfaces.Close release it.
func (b *borrowed) Close() error {
	b.log.Warn("daemon.dlog.borrow_close_ignored",
		"a borrower closed a non-closeable log sink handle; the sink is unaffected",
		Context{"sink": b.name, "fd": int(b.f.Fd())})
	return nil
}
