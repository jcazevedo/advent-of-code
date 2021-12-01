#!/usr/bin/env bash

case "$1" in
    1)
        apl --silent --Color --noCONT --noCIN --noSV --OFF -f day01-apl/day01.apl
        ;;

    *)
        echo "Unknown or unavailable day"
        exit 1
esac
