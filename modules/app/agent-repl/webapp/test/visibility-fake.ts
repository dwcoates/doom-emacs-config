import type { VisibilityWatcher } from "../src/expand.js";

/**
 * A visibility watch the suite drives: jsdom lays nothing out, so a test says
 * when a section stops (or starts) showing any part of itself.
 */
export function fakeVisibility(): {
  watcher: VisibilityWatcher;
  report: (section: HTMLElement, visible: boolean) => void;
  observed: () => HTMLElement[];
} {
  const watching = new Set<HTMLElement>();
  let on: ((section: HTMLElement, visible: boolean) => void) | undefined;
  return {
    watcher: (_root, callback) => {
      on = callback;
      return {
        observe: (section) => watching.add(section),
        unobserve: (section) => watching.delete(section),
        disconnect: () => watching.clear(),
      };
    },
    report: (section, visible) => {
      if (watching.has(section)) on?.(section, visible);
    },
    observed: () => [...watching],
  };
}

