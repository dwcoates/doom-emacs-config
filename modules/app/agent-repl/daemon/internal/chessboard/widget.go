package chessboard

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"claude-repld/internal/dlog"
)

// THE WIDGET BUILD. cee-webapp and this daemon both serve the widget from the
// checkout's own build output, sdks/cli/web/packages/cee-web-widget/dist, so
// "built" is a property of that directory. It is rebuilt when its STAMP — a
// hash of every file the build reads — differs from the stamp the last build
// left beside its output, which a fresh `vite build` (it empties dist/) or a
// changed source both cause.

// The widget package's paths, relative to the CEE CLI directory.
const (
	// webDir is the npm workspace root the widget builds in.
	webDir = "web"
	// widgetPackageDir is the widget package inside it.
	widgetPackageDir = "web/packages/cee-web-widget"
	// widgetPackageName is the package's npm name, for `npm run -w`.
	widgetPackageName = "@chesscom/cee-web-widget"
)

// The build's two output files, inside the widget's dist/.
const (
	// WidgetScript is the ES module exporting mountCeeWebWidget.
	WidgetScript = "cee-web-widget.js"
	// WidgetStylesheet is the widget's stylesheet.
	WidgetStylesheet = "cee-web-widget.css"
)

// widgetStampFile is the stamp a build leaves beside its output.
const widgetStampFile = ".agent-repl-build-stamp"

// widgetInputs are what a widget build reads, relative to the CEE CLI
// directory: single files, and directories read whole. The lockfile and the
// generated bindings are among them, so a dependency or schema change
// rebuilds the widget too.
var widgetInputs = []string{
	"web/package.json",
	"web/package-lock.json",
	"web/tsconfig.base.json",
	"web/vite.shared.ts",
	"web/gen",
	"web/packages/cee-web-widget/package.json",
	"web/packages/cee-web-widget/vite.config.ts",
	"web/packages/cee-web-widget/tsconfig.json",
	"web/packages/cee-web-widget/tsconfig.build.json",
	"web/packages/cee-web-widget/scripts",
	"web/packages/cee-web-widget/src",
}

// skippedDirs are directories never hashed: installed dependencies and build
// output are what a build PRODUCES from the inputs.
var skippedDirs = map[string]bool{"node_modules": true, "dist": true}

// widgetStamp hashes every widget input under cli: each file's path relative
// to cli and its bytes, in path order. An input absent from the checkout is
// hashed as absent, so adding one changes the stamp.
func widgetStamp(cli string) (string, error) {
	var files []string
	for _, input := range widgetInputs {
		root := filepath.Join(cli, input)
		info, err := os.Stat(root)
		if errors.Is(err, fs.ErrNotExist) {
			files = append(files, input+"\x00absent")
			continue
		}
		if err != nil {
			return "", fmt.Errorf("stat %s: %w", input, err)
		}
		if !info.IsDir() {
			files = append(files, input)
			continue
		}
		err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				if skippedDirs[d.Name()] {
					return filepath.SkipDir
				}
				return nil
			}
			rel, err := filepath.Rel(cli, path)
			if err != nil {
				return err
			}
			files = append(files, filepath.ToSlash(rel))
			return nil
		})
		if err != nil {
			return "", fmt.Errorf("walk %s: %w", input, err)
		}
	}
	sort.Strings(files)
	h := sha256.New()
	for _, rel := range files {
		h.Write([]byte(rel + "\x00"))
		if strings.HasSuffix(rel, "\x00absent") {
			continue
		}
		body, err := os.ReadFile(filepath.Join(cli, filepath.FromSlash(rel)))
		if err != nil {
			return "", fmt.Errorf("read %s: %w", rel, err)
		}
		h.Write([]byte(strconv.Itoa(len(body)) + "\x00"))
		h.Write(body)
	}
	return hex.EncodeToString(h.Sum(nil))[:16], nil
}

// widgetBuilt reports whether dist holds both output files under stamp.
func widgetBuilt(dist, stamp string) bool {
	recorded, err := os.ReadFile(filepath.Join(dist, widgetStampFile))
	if err != nil || strings.TrimSpace(string(recorded)) != stamp {
		return false
	}
	for _, name := range []string{WidgetScript, WidgetStylesheet} {
		if _, err := os.Stat(filepath.Join(dist, name)); err != nil {
			return false
		}
	}
	return true
}

// widgetDist answers the widget's build output directory under cli.
func widgetDist(cli string) string {
	return filepath.Join(cli, filepath.FromSlash(widgetPackageDir), "dist")
}

// ensureWidget builds the widget when its output is missing or stale, and
// answers the stamp it is built under. A failed build answers a failure whose
// reason is the reader-facing line.
func (b *backend) ensureWidget(ctx context.Context, cli string) (string, *failure) {
	stamp, err := widgetStamp(cli)
	if err != nil {
		return "", b.fail("widget_stamp", "The chess widget's sources could not be read.", err, dlog.Context{"cli_dir": cli})
	}
	dist := widgetDist(cli)
	if widgetBuilt(dist, stamp) {
		b.log.Debug(opBuild, "the chess widget is built from its current sources", dlog.Context{"stamp": stamp, "dist": dist})
		return stamp, nil
	}
	b.step(stepBuildingWidget)
	b.log.Info(opBuild, "building the chess widget, its build output being missing or stale", dlog.Context{"stamp": stamp, "dist": dist})
	web := filepath.Join(cli, webDir)
	for _, argv := range [][]string{
		{"npm", "ci"},
		{"npm", "run", "build", "-w", widgetPackageName},
	} {
		if f := b.runStep(ctx, "widget_build", "Building the chess widget failed", web, argv); f != nil {
			return "", f
		}
	}
	if err := os.WriteFile(filepath.Join(dist, widgetStampFile), []byte(stamp+"\n"), 0o644); err != nil {
		return "", b.fail("widget_stamp", "The chess widget was built, but its build could not be recorded.", err, dlog.Context{"dist": dist})
	}
	if !widgetBuilt(dist, stamp) {
		return "", b.fail("widget_build", "Building the chess widget produced no bundle.",
			fmt.Errorf("%s holds no %s and %s after the build", dist, WidgetScript, WidgetStylesheet), dlog.Context{"dist": dist})
	}
	b.log.Info(opBuild, "built the chess widget", dlog.Context{"stamp": stamp, "dist": dist})
	return stamp, nil
}
