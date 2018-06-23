#/bin/bash

tail -n +2 $1 | sed -e "s/data//g" | tr -d "@" | sed 's/attribute/attribute,/g' | sed 's/{/,[/g' | sed 's/}/]/g' | tr -d " \t\r" | sed '/^\s*$/d' | tr '[:upper:]' '[:lower:]' | sed 's/.*/&./' > $1.reducido
