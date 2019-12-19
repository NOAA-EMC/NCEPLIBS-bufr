 if (( $# == 0 )); then
   echo "*** Usage: $0 dell|cray|wcoss|theia|hera|gaea|jet|intel_general|gnu_general libver=<lib_ver> [debug] [compileonly] [prefix=<installpath>]" >&2
   exit 1
 fi
 sys=${1,,}
 [[ $sys == dell || $sys == cray || $sys == wcoss || \
    $sys == theia || $sys == hera || $sys == gaea || $sys == jet || \
    $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 dell|cray|wcoss|theia|hera|gaea|jet|intel_general|gnu_general libver=<lib_ver> [debug] [compileonly] [prefix=<installpath>]" >&2
   exit 1
 }

 debg=false
 inst=true
 rinst=false
 local=true
 instloc="---"
 (( nrinst = 0 ))
 (( nconfm = 0 ))
 for n in 2 3 4; do
   if (( $# > (n-1) )); then
     [[ ${!n,,} == debug ]] && debg=true
     [[ ${!n,,} == compileonly ]] && inst=false
     [[ ${!n,,} == libver=* ]] && libver=${!n:$(expr length "libver=")}
     [[ ${!n,,} == prefix=* ]] && {
       local=false
       instloc=${!n:$(expr length "prefix=")}
       [[ $instloc == /* ]] || { [[ -n $CDIR ]] && instloc=$CDIR/$instloc; }
     }
     [[ ${!n,,} == install ]] && (( nrinst = n ))
     [[ ${!n,,} == confirm:yes ]] && (( nconfm = n-1 ))
   fi
 done
 (( nrinst > 0 && nconfm == 0 )) && {
   echo "*** Info: ignore unsupported option \"install\" ***"
   (( nrinst = 0 ))
 }
 (( nrinst == 0 && nconfm > 0 )) && {
   echo "*** Info: ignore \"confirm:YES\"; must be present along with \"install\" ***"
   (( nrinst = 0 ))
 }
 (( nconfm > 0 && ! nrinst == nconfm )) && {
   echo "*** Info: ignore \"confirm:YES\"; must be present immediately after \"install\" ***"
   (( nrinst = 0 ))
 }
 (( nconfm > 0 &&  nrinst == nconfm )) && rinst=true

 $rinst && { local=false; inst=true; }

