package scan

// Calls answers one shape per SELECTOR, each taking ARGS arguments with the
// template at TEMPLATE and the record's named values at CONTEXT, and
// MsgCall: the shapes of one logging helper's level methods.
func Calls(args, template, context int, selectors ...string) []Call {
	out := make([]Call, 0, len(selectors)+1)
	for _, s := range selectors {
		out = append(out, Call{Selector: s, Args: args, Template: template, Context: context})
	}
	return append(out, MsgCall)
}

// TB is the part of testing.TB Require reports through.
type TB interface {
	Helper()
	Fatalf(format string, args ...any)
	Errorf(format string, args ...any)
}

// Require is every runtime's scan test: it scans SPEC's module and fails T
// on a scan that cannot run, on one that finds no log call (it read no
// source), and on each fault Check finds, every one reported. It answers the
// sites found.
func Require(t TB, spec Spec) []Site {
	t.Helper()
	sites, err := Scan(spec)
	if err != nil {
		t.Fatalf("scan the %s module's source: %v", spec.Module, err)
		return nil
	}
	if len(sites) == 0 {
		t.Fatalf("the scan of the %s module found no log call; it read no source", spec.Module)
		return nil
	}
	for _, fault := range Check(sites) {
		t.Errorf("%v", fault)
	}
	return sites
}
