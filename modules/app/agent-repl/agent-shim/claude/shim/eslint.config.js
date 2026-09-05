import js from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";

/**
 * The shim's linter. Type-aware ON PURPOSE, and for the same reason as the
 * webapp's: this package converts the SDK's untyped stream into typed protobuf
 * arms and serves them over a UDS, so its two standing hazards are an arm
 * nobody converted and a promise nobody awaited. `tsc --noEmit` sees neither —
 * an unmatched `case` is legal TypeScript and a dropped `await` is legal
 * everywhere.
 *
 * The rule set is `recommendedTypeChecked` plus the additions below. It is
 * kept deliberately in step with `webapp/eslint.config.js`: the two packages
 * share the generated protobuf stubs and the same class of bug, and a rule
 * that is worth enforcing on one side of the wire is worth enforcing on the
 * other. Every addition and every subtraction is argued where it is written.
 */
export default tseslint.config(
  {
    ignores: ["dist/**", "coverage/**", "node_modules/**"],
  },

  // A stale `eslint-disable` is a claim about the code that is no longer true,
  // so it fails the run rather than warning into a scrollback nobody reads.
  { linterOptions: { reportUnusedDisableDirectives: "error" } },

  js.configs.recommended,

  // ---- the type-aware program ------------------------------------------
  {
    // src/, test/ and scripts/ are exactly what tsconfig.json compiles from
    // inside this package. The shared protobuf stubs at ../../../proto/gen/ts
    // are in that program too but are NOT linted: they are generated, the
    // contract is frozen, and AGENTS.md forbids editing proto/ locally — so a
    // finding there could only ever be suppressed, never fixed.
    files: ["src/**/*.ts", "test/**/*.ts", "scripts/**/*.ts"],
    extends: [tseslint.configs.recommendedTypeChecked],
    languageOptions: {
      parserOptions: {
        project: ["./tsconfig.json"],
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      // THE CONVERSION RULE, MECHANIZED: an arm nobody converted must not
      // fall through in silence. Which of this rule's two modes delivers that
      // was MEASURED here, not assumed.
      //
      // At `considerDefaultExhaustiveForUnions: false` — a `default:` does not
      // excuse an unnamed arm — the rule reports 28 sites in this package and
      // not one is a defect: every partial switch already ends in a default
      // that throws `unsetOneof`, raises a `PersistenceError`, or logs at warn
      // and records the message as residue. Worse, the two largest are
      // switches over the SDK's own `subtype` union, which grows on the
      // vendor's schedule: pinning twenty-four case labels there would turn
      // every SDK bump into a lint failure in a file that already handles the
      // unknown kind correctly.
      //
      // At `true` — the setting used here — a switch that ends in a default
      // has made a decision, and a switch that does NOT and misses an arm is
      // an error. That is the silent fall-through: the conversion that returns
      // `undefined` because nobody wrote its case and nobody wrote a default.
      // Zero sites violate it today; the rule is the ratchet that keeps it so.
      //
      // Where a switch really is total and must stay total, the stronger tool
      // is a `const _exhaustive: never = arm` in its default — a compile
      // error, not a lint one.
      "@typescript-eslint/switch-exhaustiveness-check": [
        "error",
        { requireDefaultForNonUnion: true, considerDefaultExhaustiveForUnions: true },
      ],

      // `a || b` substitutes b for "" and 0. On this side of the wire that
      // means an empty tool result or a zero cost silently becomes a default.
      // `??` only substitutes for null/undefined.
      "@typescript-eslint/prefer-nullish-coalescing": "error",

      // `return p` inside a `try` escapes that try's `catch`: the promise
      // rejects at the CALLER, past the handler written to receive it. That is
      // error handling deleted by accident, which is exactly what this repo
      // does not allow on purpose.
      "@typescript-eslint/return-await": ["error", "in-try-catch"],

      // A rejection carrying a string loses the stack, and AGENTS.md requires
      // every error to be logged once with its cause.
      "@typescript-eslint/prefer-promise-reject-errors": "error",

      // Cheap insurance, all currently clean: keep them clean.
      "@typescript-eslint/no-unnecessary-boolean-literal-compare": "error",
      "@typescript-eslint/no-array-delete": "error",
      "@typescript-eslint/no-duplicate-type-constituents": "error",
      eqeqeq: ["error", "always", { null: "ignore" }],

      // "Direct `console` ... forbidden except the documented pre-logger
      // bootstrap failure and logger-sink emergency paths." src/ and test/ are
      // clean of it today; this keeps them that way. scripts/ is exempted
      // below — a CLI's stdout IS its output.
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
      // that is async by contract — `interrupt: async (): Promise<...> =>`,
      // `server: { close: async () => undefined }`. Dropping `async` there
      // would mean hand-wrapping each return in `Promise.resolve`, which is
      // strictly worse code written to satisfy a linter. The defect this rule
      // is reaching for (a forgotten `await`) is already covered, and covered
      // better, by no-floating-promises and await-thenable.
      "@typescript-eslint/require-await": "off",
    },
  },

  // `no-floating-promises` keeps its default `ignoreVoid: true`, so an
  // explicit `void` still marks a fire-and-forget — the spelling this package
  // already uses for the detached pumps it starts and never joins.
  //
  // `no-unnecessary-condition` is likewise off (it is a strictTypeChecked
  // rule, not enabled here). It reads a guard as redundant whenever the static
  // type says the value cannot be null — but almost every type in here is
  // decoded from the SDK or the wire, where the type states an intent the
  // bytes may not honor. Satisfying that rule means deleting defensive checks,
  // and this repo does not trade error handling for a clean lint run.


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
    files: ["test/**/*.ts", "scripts/**"],
    rules: {
      "@typescript-eslint/only-throw-error": "off",
      "@typescript-eslint/prefer-promise-reject-errors": "off",
      "require-yield": "off",
    },
  },

  // ---- the vitest configuration at the package root --------------------
  {
    // Outside tsconfig.json's program, so syntax-level rules only. They are
    // small and declarative; the type-aware rules have nothing to say.
    files: ["*.ts"],
    extends: [tseslint.configs.recommended],
    languageOptions: { globals: globals.node },
  },

  // ---- the command-line tooling ----------------------------------------
  {
    // scripts/ and its capture harness are CLIs: stdout is how they report,
    // and they run under node with no logger to route through.
    files: ["scripts/**", "*.mjs"],
    languageOptions: { globals: globals.node },
    rules: { "no-console": "off" },
  },
);
