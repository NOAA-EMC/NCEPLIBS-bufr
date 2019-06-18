 reset_version() {
  local setx_status=${-//[^x]/}
  [[ -n $setx_status ]] && set +x
    (( $# == 2 )) && {
                      local var=$1
                      local ver_name=${var^^}_VER
                      local old_ver=${!ver_name}
                      local new_ver=$2
                     } || {
                      local var=$1
                      local old_ver=$2
                      local new_ver=$3
                     }
    for vline in "$(env | grep ${var^^})"; do
      eval ${vline//${old_ver}/${new_ver}}
    done
  [[ -n $setx_status ]] && set -x
 }
