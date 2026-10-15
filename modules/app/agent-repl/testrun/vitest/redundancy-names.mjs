// redundancy-names.mjs -- how `testrun redundancy vitest`'s setup files
// (redundancy-trace.mjs, redundancy-skip.mjs) name a test, so the traced runs
// and Stryker's runs key every test alike.

// fullName is a test's "<describe > ... > test>" name.
export function fullName(task) {
  const names = [];
  for (let t = task; t && t.type !== "file" && t.name; t = t.suite) {
    names.unshift(t.name);
  }
  return names.join(" > ");
}
