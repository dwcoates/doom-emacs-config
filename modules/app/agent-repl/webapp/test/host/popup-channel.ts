/**
 * popup-channel — stand-ins for the BroadcastChannel the sidebar's popups
 * travel on (src/host/sidebar-popups.ts), for every suite that starts a
 * document holding one: a fake that records and delivers, and a quiet one that
 * carries nothing.
 */
import type { PopupChannel } from "../../src/host/sidebar-popups.js";

/** A channel that records what is posted and delivers what the test sends it. */
export function fakePopupChannel() {
  const posted: unknown[] = [];
  const listeners = new Set<(event: MessageEvent) => void>();
  const channel: PopupChannel = {
    postMessage: (message) => void posted.push(message),
    addEventListener: (_type, listener) => void listeners.add(listener),
    removeEventListener: (_type, listener) => void listeners.delete(listener),
    close: () => listeners.clear(),
  };
  const deliver = (data: unknown): void => {
    for (const listener of [...listeners]) listener(new MessageEvent("message", { data }));
  };
  return { channel, posted, deliver, listeners };
}

/** A channel that carries nothing, for a suite not about the popups. */
export function quietPopupChannel(): PopupChannel {
  return fakePopupChannel().channel;
}
