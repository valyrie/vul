(* internal diagnostics *)

type blame_pos = {source: Path.path; off: int}
type blame = {startat: blame_pos; endat: blame_pos}