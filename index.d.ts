import type { Component } from 'svelte';

declare module 'ox-svelte' {
  export const OrgComponent: Component<
    {}, // Props
    { metadata: OrgMetadata } // Exports
  >;

  export type OrgMetadata = {
    title: string | undefined;
    subtitle: string | undefined;
    author: string | undefined;
    date: string | undefined;
    description: string | undefined;
    keywords: string[] | undefined;
    language: string | undefined;
    creator: string | undefined;
  };
}
