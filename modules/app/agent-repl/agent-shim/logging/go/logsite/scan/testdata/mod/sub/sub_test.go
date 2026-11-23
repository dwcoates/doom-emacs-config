package sub

func f(l interface {
	Error(string, string, map[string]any)
}) {
	l.Error("op", "a test's call", nil)
}
