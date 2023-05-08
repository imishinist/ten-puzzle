let
  pkgs = import <nixpkgs> { };
  # choose the ocaml version you want to use
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_12;
in
pkgs.mkShell {
  # build tools
  nativeBuildInputs = with ocamlPackages; [ ocaml findlib dune_2 ocaml-lsp utop ];
  # dependencies
  buildInputs = with ocamlPackages; [ ocamlgraph ];
}
