import type { SvelteComponent } from 'svelte';

declare module 'ox-svelte' {
  export type OrgModule = {
    default: typeof SvelteComponent;
    metadata: OrgMetadata;
  };

  export type OrgMetadata = {
    title: string?;
    subtitle: string?;
    author: string?;
    date: string?;
    description: string?;
    keywords: string[]?;
    language: string?;
    creator: string?;
  };
}
