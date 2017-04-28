/**
 * So for this, OCaml doesn't actually keep any
 * type information around once compiled. This
 * is obviously not _actually_ OCaml once compiled,
 * so I can leverage the JS-interop modules, but
 * this will do for now.
 */
let isBoolean value => value == true || value == false ? true : false;
