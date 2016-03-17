ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/../
SRC_DIR="${ROOT_DIR}/src"

MODULE_DIRS=(
  "api"
  "common"
  "engine"
  "exec"
  "llvm"
  "memory"
  "optimizer"
  "parser"
  "plan"
  "repl"
  "sql"
  "storage"
  "util"
)

for module in "${MODULE_DIRS[@]}"
do
  cd "${SRC_DIR}/${module}"
  cargo test --verbose
  exit_code=$?
  if [ $exit_code != 0 ]; then
    exit $exit_code
  fi
  cd $ROOT_DIR
done
