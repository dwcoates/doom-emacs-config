/**
 * drag-pointer — the pointer every drag test drives (`src/drag.ts`), the
 * sidebar's and the feed's alike.
 */

/**
 * Take down the click guard a drag left owed (`src/drag.ts`). A drag
 * owes its window one swallowed click until a press comes; a test that ends
 * mid-gesture or with no click after its release would hand that debt to the
 * next test, or the next file sharing the window. Every file that drags calls
 * this after each test: it is the press the user's next gesture begins with.
 */
export function settleOwedDragClick(): void {
  window.dispatchEvent(new PointerEvent("pointerdown"));
}

/**
 * Dispatch a primary, left-button pointer event of TYPE at TARGET, at height
 * Y: the one pointer a drag test drives (`src/drag.ts`).
 */
export function primaryPointer(type: string, target: EventTarget, y: number): void {
  target.dispatchEvent(new PointerEvent(type, { bubbles: true, pointerId: 1, isPrimary: true, button: 0, clientX: 10, clientY: y }));
}
