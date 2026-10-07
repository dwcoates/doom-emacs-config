package chessboard

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// widgetBackend builds a backend over r with no step listener interest.
func widgetBackend(r Runner, log dlog.Logger) *backend {
	return &backend{log: log, run: r, onStep: func(string) {}, life: context.Background()}
}

func TestWidgetStampIsStableForUnchangedSources(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)

	// Act.
	first, err1 := widgetStamp(cli)
	second, err2 := widgetStamp(cli)

	// Assert.
	if err1 != nil || err2 != nil || first != second {
		t.Fatalf("stamps = %q (%v), %q (%v); want one stamp twice", first, err1, second, err2)
	}
}

func TestWidgetStampChangesWithASource(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	before, _ := widgetStamp(cli)
	writeFile(t, filepath.Join(cli, "web/packages/cee-web-widget/src/index.ts"), "export const changed = 1")

	// Act.
	after, err := widgetStamp(cli)

	// Assert.
	if err != nil || after == before {
		t.Fatalf("stamp after a source change = %q (%v), want a stamp other than %q", after, err, before)
	}
}

func TestWidgetStampChangesWithTheLockfile(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	before, _ := widgetStamp(cli)
	writeFile(t, filepath.Join(cli, "web/package-lock.json"), `{"lockfileVersion":3}`)

	// Act.
	after, err := widgetStamp(cli)

	// Assert.
	if err != nil || after == before {
		t.Fatalf("stamp after a lockfile change = %q (%v), want a stamp other than %q", after, err, before)
	}
}

func TestWidgetStampIgnoresInstalledDependencies(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	before, _ := widgetStamp(cli)
	writeFile(t, filepath.Join(cli, "web/packages/cee-web-widget/src/node_modules/x/index.js"), "x")

	// Act.
	after, err := widgetStamp(cli)

	// Assert.
	if err != nil || after != before {
		t.Fatalf("stamp after installing a dependency = %q (%v), want it unchanged at %q", after, err, before)
	}
}

func TestWidgetStampChangesWhenAnInputAppears(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	before, _ := widgetStamp(cli)
	writeFile(t, filepath.Join(cli, "web/vite.shared.ts"), "export {}")

	// Act.
	after, err := widgetStamp(cli)

	// Assert.
	if err != nil || after == before {
		t.Fatalf("stamp after an input appeared = %q (%v), want a stamp other than %q", after, err, before)
	}
}

func TestEnsureWidgetBuildsAMissingWidget(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	r := newFakeRunner()
	buildsTheWidget(r, cli)
	b := widgetBackend(r, dlog.NewTestLogger())

	// Act.
	_, f := b.ensureWidget(context.Background(), cli)

	// Assert.
	want := []string{"npm ci", "npm run build -w @chesscom/cee-web-widget"}
	if f != nil || strings.Join(r.commands(), "|") != strings.Join(want, "|") {
		t.Fatalf("ensureWidget ran %v (failure %v), want %v", r.commands(), f, want)
	}
}

func TestEnsureWidgetSkipsACurrentBuild(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	r := newFakeRunner()
	buildsTheWidget(r, cli)
	b := widgetBackend(r, dlog.NewTestLogger())
	if _, f := b.ensureWidget(context.Background(), cli); f != nil {
		t.Fatalf("first build failed: %v", f)
	}
	built := len(r.commands())

	// Act.
	_, f := b.ensureWidget(context.Background(), cli)

	// Assert.
	if f != nil || len(r.commands()) != built {
		t.Fatalf("a second ensure ran %v, want nothing beyond the first build", r.commands()[built:])
	}
}

func TestEnsureWidgetRebuildsAfterASourceChange(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	r := newFakeRunner()
	buildsTheWidget(r, cli)
	b := widgetBackend(r, dlog.NewTestLogger())
	first, _ := b.ensureWidget(context.Background(), cli)
	writeFile(t, filepath.Join(cli, "web/packages/cee-web-widget/src/mount.ts"), "export const v = 2")

	// Act.
	second, f := b.ensureWidget(context.Background(), cli)

	// Assert.
	if f != nil || second == first || len(r.commands()) != 4 {
		t.Fatalf("after a change: stamp %q→%q, commands %v (failure %v); want a rebuild", first, second, r.commands(), f)
	}
}

func TestEnsureWidgetReportsAFailedBuildWithItsLastLine(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	r := newFakeRunner()
	r.on("npm ci", func(string, []string) (string, int, error) {
		return "npm warn\nnpm error 401 Unauthorized", 1, nil
	})
	log := dlog.NewTestLogger()
	b := widgetBackend(r, log)

	// Act.
	_, f := b.ensureWidget(context.Background(), cli)

	// Assert.
	if f == nil || f.reason != "Building the chess widget failed: npm error 401 Unauthorized" {
		t.Fatalf("failure = %v, want the build's last line", f)
	}
	assertErrorRecord(t, log, "widget_build")
}

func TestEnsureWidgetRefusesABuildThatLeftNoBundle(t *testing.T) {
	// Arrange.
	_, cli := fakeCheckout(t)
	r := newFakeRunner()
	r.on("npm run build", func(string, []string) (string, int, error) {
		return "", 0, os.MkdirAll(widgetDist(cli), 0o755)
	})
	log := dlog.NewTestLogger()
	b := widgetBackend(r, log)

	// Act.
	_, f := b.ensureWidget(context.Background(), cli)

	// Assert.
	if f == nil || f.reason != "Building the chess widget produced no bundle." {
		t.Fatalf("failure = %v, want the no-bundle failure", f)
	}
	assertErrorRecord(t, log, "widget_build")
}

// assertErrorRecord asserts the canonical backend failure record for stage.
func assertErrorRecord(t *testing.T, log *dlog.TestLogger, stage string) {
	t.Helper()
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == opBackend && rec.Context["stage"] == stage && rec.Context["cause"] != "" {
			return
		}
	}
	t.Fatalf("no ERROR %s record for stage %q in %+v", opBackend, stage, log.Records())
}
