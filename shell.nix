{ 
    pkgs ? import <nixpkgs> {},
    upkgs ? import <nixos-unstable> {},
}:

pkgs.mkShell {
    buildInputs = with upkgs; [
        zig zls glfw pkgs.epoxy
    ];
}
