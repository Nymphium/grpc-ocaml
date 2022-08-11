module Tag = struct
  let ppf_str = Fun.flip Format.fprintf "%s"

  let str ~name =
    let tag = Logs.Tag.def name ppf_str in
    Logs.Tag.add tag
  ;;

  let upstream_header = Logs.Tag.(def "upstream_header" Headers.pp |> add)
  let downstream_body = Logs.Tag.(def "downstream_body" ppf_str |> add)
  let downstream_header = Logs.Tag.(def "downstream_header" Headers.pp |> add)
  let empty = Logs.Tag.empty
end
