(* type _ Effect.t += GetImg : string -> string option Effect.t *)

let resolve_images document : string -> Slipshow.asset =
  let open Vscode in
  fun s ->
    if
      Astring.String.is_infix ~affix:"://" s
      || String.starts_with ~prefix:"//" s
    then Slipshow.Remote s
    else
      let oc = Window.createOutputChannel ~name:"Slipshow" in
      let log value = OutputChannel.appendLine oc ~value in
      let s = Fpath.v s in
      let s =
        if Fpath.is_abs s then s
        else
          let open Vscode in
          let root =
            TextDocument.fileName document |> Fpath.v |> Fpath.parent
          in
          (* let _s = "/tmp/vscodetest/old_pipe_copy.png" in *)
          (* let r = *)
          (*   Vscode.Workspace.rootPath () |> function *)
          (*   | Some s -> Fpath.v s *)
          (*   | None -> Fpath.v "none" *)
          (* in *)
          Fpath.( // ) root s
      in
      let content =
        let content =
          Node.Fs.readFileSync @@ Fpath.to_string s
          |> Node.Buffer.toBase64 |> Base64.decode_exn
        in
        (* let content = Effect.perform (GetImg (Fpath.to_string s)) in *)
        log (Base64.encode content |> Result.get_ok);
        (* print_endline content; *)
        content
      in

      (* let content = "rien" in *)
      (* let () = print_endline content in *)
      let mime_of_ext = function
        | "apng" ->
            Some "image/apng" (* Animated Portable Network Graphics (APNG) *)
        | "avif" -> Some "image/avif" (*  AV1 Image File Format (AVIF) *)
        | "gif" -> Some "image/gif" (* Graphics Interchange Format (GIF) *)
        | "jpeg" ->
            Some "image/jpeg" (* Joint Photographic Expert Group image (JPEG) *)
        | "png" -> Some "image/png" (* Portable Network Graphics (PNG) *)
        | "svg+xml" -> Some "image/svg+xml" (* Scalable Vector Graphics (SVG) *)
        | "webp" -> Some "image/webp" (* Web Picture format (WEBP) *)
        | _ -> None
      in
      let mime_type = mime_of_ext (Fpath.get_ext (* Fpath.v *) s) in
      Local { mime_type; content }
(* Slipshow.Remote content *)

let slipshow_callback ~args:_ =
  print_endline "yoooOOOOOOOOOOOOOOO";
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
      let resolve_images = resolve_images document in
      let _ =
        let open Promise.Syntax in
        let+ slipshow_content =
          (* Effect.Deep.try_with *)
          (*   (fun () ->  *)
          Promise.return @@ Slipshow.convert ~resolve_images text
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
      (* let path = *)
      (*   TextDocument.fileName document *)
      (*   |> Fpath.v |> Fpath.set_ext "html" |> Fpath.to_string *)
      (* in *)
      let wb = WebviewPanel.webview panel in

      (* let disposable = *)
      (*   Window.onDidChangeActiveTextEditor () *)
      (*     ~listener:(fun _td -> *)
      (*       let _ = *)
      (*         Vscode.Window.showInformationMessage *)
      (*           ~message:"Changed Text was called" () *)
      (*       in *)
      (*       ()) *)
      (*     () *)
      (* in *)
      let update_content text =
        let resolve_images = resolve_images document in
        let delayed = Slipshow.delayed ~resolve_images text in
        let text = Slipshow.delayed_to_string delayed in
        WebView.postMessage
          (WebviewPanel.webview panel)
          (Ojs.string_to_js (* slipshow_content *) text)
      in

      let listener td =
        let document = TextDocumentChangeEvent.document td in
        let uri = TextDocument.uri document in
        let is_good = Uri.equal uri the_uri in
        let document = TextEditor.document editor in
        let text = TextDocument.getText document () in
        (* let slipshow_content = Slipshow.convert text in *)
        (* let _ = WebView.set_html wb html in *)
        let _ = if is_good then update_content text else Promise.return false in
        ()
      in
      let disposable = Workspace.onDidChangeTextDocument ~listener () in
      Vscode.ExtensionContext.subscribe extension ~disposable;
      let _slipshow_content = Slipshow.convert text in
      (* let _message = *)
      (*   let _ = Node.Fs.writeFile path ~content:slipshow_content in *)
      (*   () *)
      (*   (\* match Bos.OS.File.write path slipshow_content with *\) *)
      (*   (\* | Error (`Msg m) -> Format.sprintf "Error: %s" m *\) *)
      (*   (\* | Ok () -> "OK" *\) *)
      (* in *)
      (* (\* let _ = Vscode.Window.showInformationMessage ~message () in *\) *)
      let options = WebviewOptions.create ~enableScripts:true () in
      let () = WebView.set_options wb options in
      let html =
        Format.sprintf
          {|<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
           <title>Cat Coding</title>
           <style>
           #right-panel1.active_panel, #right-panel2.active_panel {
             z-index: 1;
           }
           #right-panel1, #right-panel2 {
             z-index: 0;
           }
</style>
</head>
           <body>
           <div id="iframes">
	     <iframe name="frame" id="right-panel1" style="width:100%%; position:absolute; top:0;bottom:0;left:0;right:0;border:0; height: 100vh"></iframe>
	     <iframe name="frame" id="right-panel2" style="width:100%%; position:absolute; top:0;bottom:0;left:0;right:0;border:0; height: 100vh"></iframe>
           </div>
           <script>%s</script>
</body>
</html>
         |}
          (* Assets.(read Index_js) *)
          [%blob "src/src-panel/vscode_previewer.bc.js"]
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
