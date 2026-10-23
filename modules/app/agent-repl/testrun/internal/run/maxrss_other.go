//go:build !darwin

package run

// maxRSSBytes reads ru_maxrss, which Linux and the BSDs report in kilobytes.
func maxRSSBytes(maxrss int64) int64 { return maxrss * 1024 }
