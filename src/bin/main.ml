let read_file document : Slipshow.file_reader =
 fun s ->
  try
    let s =
      if Fpath.is_abs s then s
      else
        let open Vscode in
        let root = TextDocument.fileName document |> Fpath.v |> Fpath.parent in
        Fpath.( // ) root s
    in
    let content =
      let content =
        Node.Fs.readFileSync @@ Fpath.to_string s
        |> Node.Buffer.toBase64 |> Base64.decode_exn
      in
      content
    in
    Ok (Some content)
  with _ -> Ok None

let slipshow_callback ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> ()
  | Some editor ->
      let document = TextEditor.document editor in
      let text = TextDocument.getText document () in
      let path =
        TextDocument.fileName document
        |> Fpath.v |> Fpath.set_ext "html" |> Fpath.to_string
      in
      let read_file = read_file document in
      let _ =
        let open Promise.Syntax in
        let+ slipshow_content =
          (* Effect.Deep.try_with *)
          (*   (fun () ->  *)
          Promise.return
          @@ Slipshow.convert ~has_speaker_view:true ~read_file text
          (* ) *)
          (* () *)
          (* { *)
          (*   effc = *)
          (*     (fun (type a) (eff : a Effect.t) -> *)
          (*       match eff with *)
          (*       | GetImg path -> *)
          (*           Some *)
          (*             (fun (k : (a, _) Effect.Deep.continuation) -> *)
          (*               let* content = Node.Fs.readFile path in *)
          (*               Effect.Deep.continue k *)
          (*                 (Some *)
          (*                    content *)
          (*                    (\* Slipshow.Remote ("YOOOO" ^ Fpath.to_string path) *\))) *)
          (*       | _ -> None); *)
          (* } *)
        in
        let _message =
          let _ = Node.Fs.writeFile path ~content:slipshow_content in
          ()
          (* match Bos.OS.File.write path slipshow_content with *)
          (* | Error (`Msg m) -> Format.sprintf "Error: %s" m *)
          (* | Ok () -> "OK" *)
        in
        (* let _ = Vscode.Window.showInformationMessage ~message () in *)
        ()
      in
      ()

let preview_callback extension ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> ()
  | Some editor ->
      let panel =
        Window.createWebviewPanel ~viewType:"slipshowPanel"
          ~title:"Slipshow preview" ~showOptions:ViewColumn.Two
      in
      let document = TextEditor.document editor in
      let the_uri = TextDocument.uri document in
      let text = TextDocument.getText document () in
      let wb = WebviewPanel.webview panel in
      let update_content =
        let timer = ref None in
        fun text ->
          let go () =
            timer := None;
            let read_file = read_file document in
            let delayed =
              Slipshow.delayed ~has_speaker_view:true ~read_file text
            in
            let text = Slipshow.delayed_to_string delayed in
            WebView.postMessage
              (WebviewPanel.webview panel)
              (Ojs.string_to_js text)
          in
          let () =
            match !timer with Some id -> Brr.G.stop_timer id | None -> ()
          in
          let i =
            Brr.G.set_timeout ~ms:500 (fun _ ->
                let _ = go () in
                ())
          in
          timer := Some i
      in

      let listener td =
        let contentChanges = TextDocumentChangeEvent.contentChanges td in
        let document = TextDocumentChangeEvent.document td in
        let uri = TextDocument.uri document in
        let is_good =
          Uri.equal uri the_uri && (not @@ List.is_empty contentChanges)
        in
        let document = TextEditor.document editor in
        let text = TextDocument.getText document () in
        let () = if is_good then update_content text in
        ()
      in
      let disposable = Workspace.onDidChangeTextDocument ~listener () in
      Vscode.ExtensionContext.subscribe extension ~disposable;
      let options = WebviewOptions.create ~enableScripts:true () in
      let () = WebView.set_options wb options in
      let html =
        Format.sprintf
          {|<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
           <title>Slipshow</title>
           <style>
           .right-panel1.active_panel, .right-panel2.active_panel {
             z-index: 1;
           }
           .right-panel1, .right-panel2 {
             z-index: 0;
             width:100%%;
             position:absolute;
             inset:0;
             border:0;
             height: 100vh;
           }
</style>
</head>
           <body>
           <div id="iframes">
           </div>
           <script>%s</script>
</body>
</html>
         |}
          (* Assets.(read Index_js) *)
          [%blob "../src-panel/vscode_previewer.bc.js"]
      in
      let _ = WebView.set_html wb html in
      let _ = update_content text in
      ()

let activate context =
  let ss_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.slipshow"
      ~callback:slipshow_callback
  in
  let prs_disposable =
    Vscode.Commands.registerCommand ~command:"slipshow.preview"
      ~callback:(preview_callback context)
  in
  Vscode.ExtensionContext.subscribe context ~disposable:ss_disposable;
  Vscode.ExtensionContext.subscribe context ~disposable:prs_disposable

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
