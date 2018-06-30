#!/bin/bash
awk '{print>"line-"NR%2}' $1
