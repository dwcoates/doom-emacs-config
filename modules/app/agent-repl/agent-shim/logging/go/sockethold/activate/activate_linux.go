//go:build linux

package activate

import "os"

// Sockets answers every socket systemd passed this process, by name.
// NAMES is unused on Linux, where the passed descriptors name themselves.
func Sockets(names []string) ([]Socket, error) {
	return fromSystemdEnv(os.Getenv, os.Getpid(), os.NewFile)
}
