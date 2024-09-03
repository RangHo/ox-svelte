# Org-mode exporter backend for Svelte components

`ox-svelte` is an opinionated Org-mode exporter backend that generates a standalone [Svelte](https://svelte.dev/) component.

While `ox-html` *technically* generates a valid Svelte component, the resulting file includes a lot of junk.
They are useful for standalone HTML documents, but not for components.

`ox-svelte` fixes this by performing minimum preprocessing and postprocessing and exposing metadata as a module-level JavaScript property.
