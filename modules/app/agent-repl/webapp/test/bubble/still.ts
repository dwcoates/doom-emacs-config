/**
 * THE STILL BUBBLE (owner ruling, 2026-10-26): no bubble runs an animation of
 * its own, so none carries the retired thinking wave's marker or an inline
 * style (the wave's phase rode there as a negative `animation-delay`). The one
 * reading of that, so every suite asks the same question.
 */
export function isStill(bubble: Element): boolean {
  return !bubble.hasAttribute("data-wave") && !bubble.hasAttribute("style");
}
