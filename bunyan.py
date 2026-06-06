from __future__ import annotations

import shutil

from scripts.bunyan import BuildContext, cmake_build, cmake_configure, main, run_cmd


def build_haikal(ctx: BuildContext) -> None:
    haikal_build_dir = ctx.root_dir / "extern" / "haikal" / "build"
    haikal_build_dir.mkdir(parents=True, exist_ok=True)

    cmake_configure(
        source_dir=ctx.root_dir / "extern" / "haikal",
        build_dir=haikal_build_dir,
        generator=ctx.generator,
        c_compiler=ctx.c_compiler,
        build_type="Release",
    )
    cmake_build(haikal_build_dir)
    run_cmd(
        [
            haikal_build_dir / "haikal.exe",
            "--entry",
            ctx.root_dir / "src" / "main.c",
            "--meta",
            ctx.root_dir / "extern" / "haikal" / "src" / "meta_arena",
        ],
        cwd=ctx.root_dir,
    )


def package_i(ctx: BuildContext) -> None:
    package_dir = ctx.root_dir.parent / "i-windows-x64"
    package_std_dir = package_dir / "std"

    package_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(ctx.exe_path, package_dir / "I.exe")

    if package_std_dir.exists():
        shutil.rmtree(package_std_dir)
    shutil.copytree(ctx.root_dir / "src" / "std", package_std_dir)

    print(f"package {ctx.exe_path} -> {package_dir / 'I.exe'}")
    print(f"package {ctx.root_dir / 'src' / 'std'} -> {package_std_dir}")


if __name__ == "__main__":
    main(
        project_name="I",
        hooks={
            "pre_build": build_haikal,
            "post_build": package_i,
        },
        extra_clean_paths=(
            "extern/haikal/build",
        ),
    )
