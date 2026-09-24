//go:build !darwin

package dirpath

// onDiskPath answers path unchanged off darwin: the daemon's other hosts use
// case-sensitive volumes, where the spelling that resolved IS the stored one.
func onDiskPath(path string) (string, error) { return path, nil }
