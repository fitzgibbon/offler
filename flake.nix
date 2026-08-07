{
  description = "offler: a unified graphics and windowing library for Idris 2";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      devShells = forAll (pkgs:
        let
          # wgpu-native ships no pkg-config file, and nixpkgs splits its headers
          # into .dev while the .so stays in the default output. SDL3 is split
          # the same way but does ship sdl3.pc, so pkg-config finds it.
          wgpu = pkgs.wgpu-native;
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              idris2
              idris2Packages.pack
              chez
              nodejs
              pkg-config
              # The native backend
              sdl3
              wgpu-native
              vulkan-loader
              # Image decoding in the C shim (stb_image.h)
              stb
              # Software Vulkan (lavapipe) and a headless X server, so the
              # native examples can be smoke-tested without a GPU or a display
              mesa
              vulkan-tools
              xvfb
              imagemagick
              xdotool
            ];

            # Consumed by the Makefile, which falls back to WGPU_PREFIX or
            # pkg-config when these are unset.
            WGPU_CFLAGS = "-I${wgpu.dev}/include";
            WGPU_LIBS = "-L${wgpu}/lib -lwgpu_native -Wl,-rpath,${wgpu}/lib";
            STB_CFLAGS = "-I${pkgs.stb}/include/stb";

            shellHook = ''
              # wgpu dlopens libvulkan.so.1 at run time. Without the loader on
              # the library path it reports "vulkan drivers/libraries could not
              # be loaded" and finds no adapter at all.
              export LD_LIBRARY_PATH="${pkgs.vulkan-loader}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

              echo "$(idris2 --version)"
              echo
              echo "  make            every example: browser bundles and native binaries"
              echo "  make web        just the bundles; then open examples/*/index.html"
              echo "  make native     just the binaries"
              echo "  make check      the programs that must fail to compile"
              echo
              echo "For a GPU-less run of a native example:"
              echo "  export VK_ICD_FILENAMES=\$(ls /run/opengl-driver/share/vulkan/icd.d/lvp_icd.*.json 2>/dev/null | head -1)"
            '';
          };
        });

      formatter = forAll (pkgs: pkgs.nixpkgs-fmt);
    };
}
