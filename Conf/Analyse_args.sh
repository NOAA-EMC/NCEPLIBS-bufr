 if (( $# == 0 )); then
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [prefix=<installpath>] [[local]install[only]]" >&2
   exit 1
 fi

 sys=${1,,}
 [[ $sys == wcoss || $sys == dell || $sys == cray ||\
    $sys == theia || $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [prefix=<installpath>] [[local]install[only]]" >&2
   exit 1
 }
 debg=false
 inst=false
 skip=false
 local=false
 instloc="---"
 for n in 2 3 4; do
   if (( $# > (n-1) )); then
     [[ ${!n,,} == build ]] && debg=false
     [[ ${!n,,} == debug ]] && debg=true
     [[ ${!n,,} == install ]] && inst=true
     [[ ${!n,,} == localinstall ]] && { local=true; inst=true; }
     [[ ${!n,,} == installonly ]] && { inst=true; skip=true; }
     [[ ${!n,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
     [[ ${!n,,} == prefix=* ]] && {
       local=false
       instloc=${!n:$(expr length "prefix=")}
     }
   fi
 done
