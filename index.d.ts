import type { Component } from 'svelte';

declare module 'ox-svelte' {
  /// Type of the imported Svelte "module" using `ox-svelte`.
  export type OrgModule = {
    metadata: OrgMetadata;
    default: Component;
  };
  
  /// Metadata extracted from Org file.
  ///
  /// There are some well-known properties in an Org file that require special
  /// type handling. If a property is not one of the well-known properties, it
  /// is assumed to be a single string.
  export interface OrgMetadata {
    /// Title of the Org file, i.e. the `#+TITLE:` property.
    ///
    /// This property is normalized into a single string.
    readonly title?: string;

    /// Subtitle of the Org file, i.e. the `#+SUBTITLE:` property.
    ///
    /// This property is normalized into a single string.
    readonly subtitle?: string;

    /// Description of the Org file, i.e. the `#+DESCRIPTION:` property.
    ///
    /// This property is normalized into a single string.
    readonly description?: string;

    /// Date or timestamp associated with the Org file, i.e. the `#+DATE:`
    /// property.
    ///
    /// This property is transformed with `org-export-get-date` function. Note
    /// that Emacs's "ISO 8601" format is slightly different, and its form is
    /// defined below. The built-in `Date` constructor should have no problem
    /// parsing the date, however.
    readonly date?: ISO8601;

    /// Authors of the Org file, i.e. the `#+AUTHOR:` property.
    ///
    /// This property is not normalized, in case there are multiple authors.
    readonly authors?: string[];

    /// Email of the Org file, i.e. the `#+EMAIL:` property.
    ///
    /// This property is not normalized, in case there are multiple authors.
    readonly email?: string[];

    /// Keywords of the Org file, i.e. the `#+KEYWORDS:` property.
    ///
    /// This property is not normalized, in case there are multiple keywords.
    readonly keywords?: string[];

    /// Category of the Org file, i.e. the `#+CATEGORY:` property.
    ///
    /// This property is normalized into a single string.
    readonly category?: string;

    /// Content language of the Org file, i.e. the `#+LANGUAGE:` property.
    readonly language?: string;
    
    /// Other properties of the Org file.
    readonly [key: string]: any;
  }

  type ISO8601Year = `${number}${number}${number}${number}`;
  type ISO8601Month = `${number}${number}`;
  type ISO8601Day = `${number}${number}`;
  type ISO8601Hour = `${number}${number}`;
  type ISO8601Minute = `${number}${number}`;
  type ISO8601Second = `${number}${number}`;
  type ISO8601Timezone = "Z" | `${"-" | "+"}${ISO8601Hour}${ISO8601Minute}`;

  /// "ISO 8601" date format used by Emacs.
  ///
  /// It differs from the standard ISO 8601 format in that the timezone hour and
  /// minute are not separated by a colon.
  export type ISO8601 = `${ISO8601Year}-${ISO8601Month}-${ISO8601Day}T${ISO8601Hour}:${ISO8601Minute}:${ISO8601Second}${ISO8601Timezone}` & string;
}
