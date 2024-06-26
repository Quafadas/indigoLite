outDir := justfile_directory() + "/.out"
testDir := justfile_directory() + "/test"


setup-ide:
  scala-cli setup-ide .

dev:
  cs launch io.github.quafadas:live-server-scala-cli-js_3:0.0.20 -- --path-to-index-html {{invocation_directory()}}/static

copyAssets:
  cp -r {{justfile_directory()}}/static/. {{outDir}}

## Builds the front end project
buildJs:
  mkdir -p {{outDir}}
  scala-cli --power package . -o {{outDir}} -f --js-mode release

format:
  scala-cli fmt .