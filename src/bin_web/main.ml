(* A version with less features to be able to run it in webIDE *)

let resolve_images _document : Slipshow.file_reader = fun _s -> Ok None

let slipshow_callback ~args:_ =
  let _ =
    Vscode.Window.showInformationMessage
      ~message:"Compiling is not possible in (yet) in the Web version of VSCode"
      ()
  in
  Ojs.unit_to_js ()

let preview_callback extension ~args:_ =
  let open Vscode in
  match Window.activeTextEditor () with
  | None -> Ojs.unit_to_js ()
  | Some editor ->
      let panel =
        Window.createWebviewPanel ~viewType:"slipshowPanel"
          ~title:"Slipshow preview" ~showOptions:ViewColumn.Two
      in
      let document = TextEditor.document editor in
      let the_uri = TextDocument.uri document in
      let text = TextDocument.getText document () in
      let wb = WebviewPanel.webview panel in
      let update_content text =
        let resolve_images = resolve_images document in
        let entry_point = Fpath.v "-" in
        let read_file f =
          if Fpath.equal entry_point f then Ok (Some text) else resolve_images f
        in
        let delayed, _warnings =
          Slipshow.delayed ~has_speaker_view:true ~read_file entry_point
        in
        (* TODO: handle warnings *)
        let text = Slipshow.delayed_to_string (delayed, "") in
        WebView.postMessage (WebviewPanel.webview panel) (Ojs.string_to_js text)
      in

      let listener td =
        let document = TextDocumentChangeEvent.document td in
        let uri = TextDocument.uri document in
        let is_good = Uri.equal uri the_uri in
        let document = TextEditor.document editor in
        let text = TextDocument.getText document () in
        let _ = if is_good then update_content text else Promise.return false in
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
</head>
           <body>
           <div id="iframes" style="position:absolute;inset:0;">
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
      Ojs.unit_to_js ()

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

(* I had to update vscode-ocaml-platform to support latest ocaml, but its API
   has changed, and I was forced to change the callback from returning unit to
   returning Ojs.t. I have implemented something, but did not test. If something
   breaks, look there, me from the future! *)
