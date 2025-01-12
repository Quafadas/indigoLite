outDir := justfile_directory() + "/.out"
testDir := justfile_directory() + "/test"


setup-ide:
  scala-cli setup-ide .

dev:
  cs launch io.github.quafadas::sjsls:0.2.1 -- --path-to-index-html {{invocation_directory()}}/static

## Builds the front end project
buildJs:
  echo "@@@"
  echo {{outDir}}
  echo "@@@"
  mkdir -p {{outDir}}
  scala-cli --power package . -o {{outDir}} -f
  ls -alR {{outDir}}
  echo "@@@"

## JP 23/06/2024 switched to scalafmt during skype call with Simon
format:
  scalafmt ~/GIT/indigoLite

## JP 24/06/2024 added "clean", sometimes help with browser synchronisation with build
clean:
  scala-cli clean .

## JP 19/09/2024 removed trailing /
## ... the previous command was ...
## cp -r {{justfile_directory()}}/static/ {{outDir}} ... which might be wrong I think
copyAssets:
  cp -r {{justfile_directory()}}/static/. {{outDir}}

## JP 19/09/2024 added listOurDir command to help debug continuous integration problem
listOutDir:
  pwd
  echo "@@@"
  echo {{outDir}}
  echo "@@@@"
  echo "/home/runner/work/indigoLite/indigoLite/.out/"
  echo "@@@@@"
##  ls -AlR {{outDir}}
##  ls -AlR {{outDir}} | wc -l
##  ls -AlR /home/runner/work/indigoLite/indigoLite/.out/
##  ls -AlR /home/runner/work/indigoLite/indigoLite/.out/ | wc -l
##  echo "@@@@@@"

ghaBuild:
  rm -rf {{outDir}}
  just buildJs
  just copyAssets;
  cs launch io.github.quafadas::sjsls:0.2.1 -- --project-dir {{outDir}} --build-tool none