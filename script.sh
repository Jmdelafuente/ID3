#/bin/bash

less $1 | tr -d " \t\r" | tr -d "@" | tr '[:upper:]' '[:lower:]' | sed -e "s/attribute//g" | sed -e "s/data//g" > $1.reducido
