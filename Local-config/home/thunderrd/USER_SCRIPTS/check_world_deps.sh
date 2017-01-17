#!/bin/bash
# About: Clean up your worldfile by removing entries which are dependencies of
#        other packages, speak: Which have reverse dependencies.
#
# Usage: You may provide the '-v' switch to get more output.

WORLDFILE=/var/lib/portage/world
VERBOSE=false
SUGGESTED_PKGS=""   # suggested for removal

# Check if verbose flag was specified
[[ $1 = "-v" ]] && VERBOSE=true

# Find removeable packages
for PKG in `cat $WORLDFILE`; do
   REVDEPS=`qdepends -C -Q $PKG`

   if [[ -n $REVDEPS ]]; then
      $VERBOSE && echo "${PKG}:"
      for DEP in $REVDEPS; do
         $VERBOSE && echo -e "\t $DEP"
      done
      SUGGESTED_PKGS="$SUGGESTED_PKGS $PKG"
   fi
done

# Summary, what may be removed?
if [[ -z $SUGGESTED_PKGS ]]; then
   echo "Couldn't find any reverse dependencies. Your world file is minimal!"
   exit
else
   $VERBOSE && echo
   echo -e "Suggested for removal:"
   for SUGGEST in $SUGGESTED_PKGS; do
      echo -e "\t $SUGGEST"
   done
fi

# Actually remove world file entries
echo
echo    "WARNING: Be shure to have a *recent* copy of your worldfile!"
echo -n "Do you want to remove all suggest now? (y/n) "
read -n1 ANSWER

if [[ $ANSWER = "y" ]] || [[ $ANSWER = "Y" ]]; then
   # Slash escaping is - as always - rather obscure.
   # Here we find the slash and replace it by backslash and slash: / -> \/
   SUGGESTED_PKGS=`echo $SUGGESTED_PKGS | sed 's/\//\\\\\//g'`
   for SUGGESTED in $SUGGESTED_PKGS; do
      sed -i "/$SUGGESTED/d" $WORLDFILE
   done
fi 
