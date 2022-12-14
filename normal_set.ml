let rec is_normal_set (s: string) : bool =
  let rec is_prefix (pre: string) (s: string) : bool =
    if String.length pre > String.length s then false
    else if pre = String.sub s 0 (String.length pre) then true
    else false
  in
  let rec is_suffix (suf: string) (s: string) : bool =
    if String.length suf > String.length s then false
    else if suf = String.sub s (String.length s - String.length suf) (String.length suf) then true
    else false
  in
  let rec check_norm (pre: string) (suf: string) (s: string) : bool =
    if pre = suf then true
    else if is_prefix pre s && is_suffix suf s then check_norm (pre ^ "a") (suf ^ "a") s
    else false
  in
  check_norm "a" "a" s
