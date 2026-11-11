package reuse

import (
	"slices"
	"strings"
	"testing"
)

func TestSpecifiers(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{"a static import", `import { a } from "./a.js";`, []string{"./a.js"}},
		{"a re-export", `export * from '../b';`, []string{"../b"}},
		{"a side-effect import", `import "./setup";`, []string{"./setup"}},
		{"a dynamic import", `await import( "./lazy" )`, []string{"./lazy"}},
		{"a require", `const x = require("./cjs")`, []string{"./cjs"}},
		{"a mocked module", `vi.mock("./dep", () => ({}))`, []string{"./dep"}},
		{"a file URL", `new URL("./fixture.json", import.meta.url)`, []string{"./fixture.json"}},
		{"a bare package", `import { it } from "vitest";`, []string{"vitest"}},
		{"each once, in order", `import "./b"; import "./a"; import "./b";`, []string{"./b", "./a"}},
		{"a computed path names nothing", "import(`./${name}`)", nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Specifiers([]byte(tt.src))

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("Specifiers = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestResolve(t *testing.T) {
	root := writeTree(t, map[string]string{
		"p/src/a.ts":         "",
		"p/src/b.tsx":        "",
		"p/src/c.mts":        "",
		"p/src/d/index.ts":   "",
		"p/src/e.js":         "",
		"p/src/style.css":    "",
		"p/src/data.json":    "",
		"p/src/real.js":      "",
		"p/src/real.ts":      "",
		"p/src/twinned.jsx":  "",
		"p/src/twinned2.tsx": "",
	})
	tests := []struct {
		spec, want string
		wantErr    bool
	}{
		{spec: "./a", want: "p/src/a.ts"},
		{spec: "./a.js", want: "p/src/a.ts"},
		{spec: "./b.jsx", want: "p/src/b.tsx"},
		{spec: "./c.mjs", want: "p/src/c.mts"},
		{spec: "./d", want: "p/src/d/index.ts"},
		{spec: "./e", want: "p/src/e.js"},
		{spec: "./real.js", want: "p/src/real.js"},
		{spec: "./style.css?raw", want: "p/src/style.css"},
		{spec: "./data", want: "p/src/data.json"},
		{spec: "./missing"},
		{spec: "vitest"},
		{spec: "../../../../outside", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.spec, func(t *testing.T) {
			// Arrange
			g := NewImportGraph(root)

			// Act
			got, ok, err := g.resolve("p/src", tt.spec)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, want error %v", err, tt.wantErr)
			}
			if ok != (tt.want != "") || got != tt.want {
				t.Fatalf("resolve = %q %v, want %q", got, ok, tt.want)
			}
		})
	}
}

func TestClosureFollowsLocalImportsTransitivelyAndOnce(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"p/test/x.test.ts": `import { a } from "../src/a"; import { it } from "vitest";`,
		"p/src/a.ts":       `import { b } from "./b"; import css from "./a.css?raw";`,
		"p/src/b.ts":       `import { a } from "./a";`,
		"p/src/a.css":      `@import "./never-followed.css";`,
		"p/src/other.ts":   "",
	})
	g := NewImportGraph(root)

	// Act
	got, err := g.Closure([]string{"p/test/x.test.ts"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"p/src/a.css", "p/src/a.ts", "p/src/b.ts", "p/test/x.test.ts"}
	if !slices.Equal(got, want) {
		t.Fatalf("Closure = %q, want %q", got, want)
	}
}

func TestClosureFailures(t *testing.T) {
	tests := []struct {
		name  string
		files map[string]string
		entry string
		want  string
	}{
		{"an entry that is not a file", map[string]string{"p/a.ts": ""}, "p/gone.ts", "is not a file"},
		{"an import out of the checkout", map[string]string{"p/a.ts": `import "../../x"`}, "p/a.ts", "resolves outside the checkout"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			g := NewImportGraph(writeTree(t, tt.files))

			// Act
			_, err := g.Closure([]string{tt.entry})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestSetupFilesAreReadFromTheConfigAndTheConfigsItImports(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"p/vitest.integration.config.ts": `import base from "./vitest.config"; export default { test: { setupFiles: ["./test/i-setup.ts"] } }`,
		"p/vitest.config.ts":             `export default { test: { setupFiles: ["./test/setup.ts", './test/worker.ts'] } }`,
		"p/test/setup.ts":                "",
		"p/test/worker.ts":               "",
		"p/test/i-setup.ts":              "",
	})
	g := NewImportGraph(root)

	// Act
	got, err := g.SetupFiles("p/vitest.integration.config.ts")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"p/test/setup.ts", "p/test/worker.ts", "p/test/i-setup.ts"}
	if !slices.Equal(got, want) {
		t.Fatalf("SetupFiles = %q, want %q", got, want)
	}
}

func TestAMissingSetupFileIsAnError(t *testing.T) {
	// Arrange
	g := NewImportGraph(writeTree(t, map[string]string{"p/vitest.config.ts": `export default { test: { setupFiles: ["./gone.ts"] } }`}))

	// Act
	_, err := g.SetupFiles("p/vitest.config.ts")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names the setup file p/gone.ts") {
		t.Fatalf("err = %v", err)
	}
}
