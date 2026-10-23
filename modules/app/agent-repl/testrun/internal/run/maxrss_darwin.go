package run

// maxRSSBytes reads ru_maxrss, which Darwin reports in bytes.
func maxRSSBytes(maxrss int64) int64 { return maxrss }
