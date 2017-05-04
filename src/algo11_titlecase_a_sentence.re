let titleCase sentence =>
  Js.String.splitByRe [%re "/\\s+/"] sentence |> Js.Array.map (fun wd => String.capitalize wd) |> Js.Array.join;
