CUR_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SRC_DIR="${CUR_DIR}/src"

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
  cargo fmt
  cd $CUR_DIR
done
