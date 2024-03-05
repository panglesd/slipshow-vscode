let () = print_endline "yoOOOOOOOOOOOO"
let div = Brr.El.find_first_by_selector (Jstr.v "#iframes") |> Option.get
let previewer = Previewer.create_previewer div

let _ =
  Brr.Ev.listen Brr_io.Message.Ev.message
    (fun event ->
      let source =
        Brr_io.Message.Ev.source (Brr.Ev.as_type event) |> Option.get
      in
      let source_name = Jv.get source "name" |> Jv.to_jstr in
      if Jstr.equal source_name (Jstr.v "frame") then ()
      else
        let raw_data : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
        let src = Jv.to_string raw_data (* "# abcd" *) in
        Previewer.preview previewer src)
    (Brr.Window.as_target Brr.G.window)
