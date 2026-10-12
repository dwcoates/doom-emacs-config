package buildid

import (
	"os"
	"syscall"
)

func statIdentity(info os.FileInfo) (string, error) {
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return "", errNoStat
	}
	return statIdentityFrom(uint64(st.Dev), st.Ino, st.Size,
		st.Mtimespec.Sec, st.Mtimespec.Nsec, st.Ctimespec.Sec, st.Ctimespec.Nsec), nil
}
