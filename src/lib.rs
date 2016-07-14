#![crate_type = "lib"]
#![crate_name = "svg_util"]

//! This crate provides some helper data structures and functions for working with SVG files.
//!
//! This is restricted to the non-xml parts of SVG, as you may want to represent the XML/DOM tree
//! differently depending on the use case.

pub mod primitive;
pub mod path;
mod util;