import js from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";

/**
 * The webapp's linter. Type-aware ON PURPOSE: this package's whole job is to
 * switch over protobuf oneofs and fire RPCs, and the two defect classes that
 * costs us — an arm nobody drew and a promise nobody awaited — are invisible
 * to a syntax-only linter. `tsc --noEmit` does not see either: an unmatched
 * `case` is legal TypeScript, and a dropped `await` is legal everywhere.
 *
 * The rule set is `recommendedTypeChecked` plus the additions below. Every
 * addition and every subtraction is argued where it is written; a rule with no
 * argument behind it does not belong in this file.
 */
export default tseslint.config(
  {
    ignores: ["dist/**", "coverage/**", "node_modules/**"],
  },

  // A stale `eslint-disable` is a claim about the code that is no longer true,
  // so it fails the run rather than warning into a scrollback nobody reads.
  { linterOptions: { reportUnusedDisableDirectives: "error" } },

  js.configs.recommended,

  // ---- the type-aware program: src/ and test/, exactly what tsconfig.json
  // compiles ------------------------------------------------------------
  {
    files: ["src/**/*.ts", "test/**/*.ts"],
    extends: [tseslint.configs.recommendedTypeChecked],
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.json"],
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      // THE HOUSE RULE, MECHANIZED: an arm nobody drew must not fall through
      // in silence. What that means here was MEASURED, not assumed, because
      // this rule's `considerDefaultExhaustiveForUnions` option decides which
      // of two very different rules you get, and the wrong one is worthless.
      //
      // At `false` — a `default:` clause does not excuse an unnamed arm — the
      // rule reports 48 sites across this package and the shim, and not one of
      // them is a defect: EVERY partial switch in both packages already ends
      // in a default that throws, logs at warn, or makes a documented
      // forward-compatible choice (`drawFooterSubStatus`: "an arm this build
      // has never heard of still draws correctly"; `armBreathes`: "everything
      // else is still"). Satisfying it would mean naming four cross-cutting
      // refusal arms at thirteen call sites that deliberately delegate them,
      // and spelling out thirty-seven `case` labels that all return the same
      // cell — plus roughly nineteen inline disables where neither is
      // defensible. A rule paid for in scattered disables is not enforcing
      // anything.
      //
      // At `true` — the setting used here — a switch that ends in a default
      // has made a decision, and a switch that does NOT and misses an arm is
      // an error. That is precisely the silent fall-through: the arm that
      // returns `undefined` because nobody wrote its case and nobody wrote a
      // default. Zero sites violate it today, which is the discipline this
      // package already keeps; the rule is the ratchet that keeps it.
      //
      // If a future arm needs catching AT the loud default rather than in it,
      // the stronger tool is a `const _exhaustive: never = arm` in that
      // default — a compile error, not a lint one — and it costs nothing to
      // add at a switch that really is total.
      "@typescript-eslint/switch-exhaustiveness-check": [
        "error",
        { requireDefaultForNonUnion: true, considerDefaultExhaustiveForUnions: true },
      ],

      // `a || b` substitutes b for "" and 0, which for this renderer means a
      // legitimately empty title or a real count of zero silently becomes
      // someone's placeholder. `??` only substitutes for null/undefined.
      "@typescript-eslint/prefer-nullish-coalescing": "error",

      // `return p` inside a `try` escapes that try's `catch`: the promise
      // rejects at the CALLER, past the handler written to receive it. That is
      // error handling deleted by accident, which is exactly what this repo
      // does not allow on purpose.
      "@typescript-eslint/return-await": ["error", "in-try-catch"],

      // A rejection carrying a string loses the stack, and every failure in
      // this package is logged with its cause.
      "@typescript-eslint/prefer-promise-reject-errors": "error",

      // Cheap insurance, all currently clean: keep them clean.
      "@typescript-eslint/no-unnecessary-boolean-literal-compare": "error",
      "@typescript-eslint/no-array-delete": "error",
      "@typescript-eslint/no-duplicate-type-constituents": "error",
      eqeqeq: ["error", "always", { null: "ignore" }],

      // "LOGGING goes through src/log.ts only." The two documented exceptions
      // get a scoped override below; everywhere else this is now enforced
      // rather than remembered.
      "no-console": "error",

      // A deliberately unused binding is spelled with a leading underscore
      // here (destructured arms, signature-conformance parameters). The
      // convention predates the linter; this teaches the linter the
      // convention rather than rewriting several dozen call sites to suit it.
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          varsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
        },
      ],

      // OFF, and not by oversight.
      //
      // `require-await` flags an `async` function whose body never awaits.
      // Nearly every hit here is a fake or an adapter satisfying an interface
      // that is async by contract — `terminalFactory: async () => terminal`,
      // `revealRow: async () => false`. Dropping `async` there would mean
      // hand-wrapping each return in `Promise.resolve`, which is strictly
      // worse code written to satisfy a linter. The defect this rule is
      // reaching for (a forgotten `await`) is already covered, and covered
      // better, by no-floating-promises and await-thenable.
      "@typescript-eslint/require-await": "off",
    },
  },

  // `no-floating-promises` keeps its default `ignoreVoid: true`, so an
  // explicit `void` still marks a fire-and-forget. That is deliberate: `void
  // guardMalformed(...)` IS the house's fire-and-forget spelling, the guard
  // is where the rejection is handled, and `void` makes every such site
  // greppable. Turning ignoreVoid off would flag the handled cases and the
  // unhandled ones identically, which tells you nothing.
  //
  // `no-unnecessary-condition` is likewise off (it is a strictTypeChecked
  // rule, not enabled here). It reads a guard as redundant whenever the static
  // type says the value cannot be null — but most of those types come from
  // decoded wire data, where the type states an intent the bytes may not
  // honor. Satisfying that rule means deleting defensive checks, and this repo
  // does not trade error handling for a clean lint run.


  // ---- the suites' fault injection ------------------------------------
  {
    // THREE RULES ARE OFF IN TEST CODE, and this is the one place in either
    // config where a rule is scoped rather than global, so it gets the whole
    // argument.
    //
    // `only-throw-error` and `prefer-promise-reject-errors` exist because a
    // rejection carrying a string loses its stack. That is right for PRODUCING
    // code and stays on everywhere it produces. In the suites the non-Error
    // rejection is the SUBJECT: `Promise.reject("the store socket went away")`,
    // `throw "EBADF"`, `throw "the volume went away"` are fakes standing in for
    // a socket, a syscall and a mount that failed in a way nobody typed. The
    // production code they drive is written defensively for exactly that —
    // `err instanceof Error ? err.message : ...` appears all over — and a suite
    // forced to inject `new Error(...)` would never once execute the other side
    // of those branches. Enforcing the rules here would DELETE error-handling
    // coverage in the name of error handling.
    //
    // `require-yield` flags an `async function*` whose body never yields. Every
    // hit is a stream fake: one that stays open forever (`await new
    // Promise<never>`), one that ends having pushed nothing, one that refuses
    // at the first `next()`. A generator with no `yield` is what those three
    // conditions ARE, and the alternative is a `yield` nobody wants reached.
    //
    // Scoped, not global: `src/` — the mocked vendor in `src/fake/` included —
    // keeps all three.
    files: ["test/**/*.ts"],
    rules: {
      "@typescript-eslint/only-throw-error": "off",
      "@typescript-eslint/prefer-promise-reject-errors": "off",
      "require-yield": "off",
    },
  },

  // ---- the two documented pre-logger sites ----------------------------
  {
    // src/log.ts IS the logger: it is where console finally gets called.
    // src/main.ts calls it once during boot, before the logger exists — the
    // bootstrap path AGENTS.md names as the single exception.
    files: ["src/log.ts", "src/main.ts"],
    rules: { "no-console": "off" },
  },

  // ---- the build/test configuration at the package root ---------------
  {
    // These are outside tsconfig.json's program, so they get the syntax-level
    // rules only. They are small and declarative; the type-aware rules have
    // nothing to say about them.
    files: ["*.ts", "*.mjs"],
    extends: [tseslint.configs.recommended],
    languageOptions: { globals: globals.node },
  },

  // ---- the developer tooling in bin/ ----------------------------------
  {
    // bin/screenshot.mjs drives a headless browser: it runs under node, and
    // the callbacks it ships into the page run under the browser. Both global
    // sets are genuinely in scope in the one file, so both are declared.
    files: ["bin/**/*.mjs"],
    languageOptions: { globals: { ...globals.node, ...globals.browser } },
  },
);
