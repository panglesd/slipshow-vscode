let () = print_endline "yoOOOOOOOOOOOO"
let div = Brr.El.find_first_by_selector (Jstr.v "#iframes") |> Option.get

let warnings =
  Brr.El.find_first_by_selector (Jstr.v "#warnings-slipshow") |> Option.get

let warnings_show =
  Brr.El.find_first_by_selector (Jstr.v "#warnings-slipshow-show") |> Option.get

let _unlistener =
  Brr.Ev.listen Brr.Ev.click
    (fun _ ->
      let show_class = Jstr.v "hide-warnings" in
      Brr.El.set_class show_class
        (not @@ Brr.El.class' show_class warnings)
        warnings)
    (Brr.El.as_target warnings_show)

let previewer =
  Previewer.create_previewer ~include_speaker_view:false ~errors_el:warnings
    ~steal_focus:false div

let _ =
  Brr.Ev.listen Brr_io.Message.Ev.message
    (fun event ->
      let source =
        Brr_io.Message.Ev.source (Brr.Ev.as_type event) |> Option.get
      in
      let source_name = Jv.get source "name" |> Jv.to_jstr in
      let f1, f2 = Previewer.ids previewer in
      if
        Jstr.equal source_name (Jstr.v f1) || Jstr.equal source_name (Jstr.v f2)
      then ()
      else
        let raw_data : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
        let src =
          try Jv.to_string raw_data (* "# abcd" *) |> Slipshow.string_to_delayed
          with _ ->
            Brr.Console.(log [ raw_data ]);
            failwith "Ici"
        in
        Previewer.preview_compiled previewer src)
    (Brr.Window.as_target Brr.G.window)
