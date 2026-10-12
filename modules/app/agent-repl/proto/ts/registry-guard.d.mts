// Types for registry-guard.mjs, the build-time protobuf-es registry guard.
export declare const REGISTRY_FILE: RegExp;
export declare function guardRegistrySource(source: string): string;
export declare function esbuildRegistryGuard(): { name: string; setup(build: unknown): void };
export declare function viteRegistryGuard(): {
  name: string;
  enforce: "pre";
  transform(code: string, id: string): { code: string; map: null } | null;
};
