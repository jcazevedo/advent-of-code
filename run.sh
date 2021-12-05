#!/usr/bin/env bash

case "$1" in
    1)
        apl --silent --Color --noCONT --noCIN --noSV --OFF -f day01-apl/day01.apl
        ;;

    2)
        cd day02-elm && elm make Main.elm --optimize --output main.js && cd .. && node day02-elm/run.js
        ;;

    3)
        fish day03-fish/day03.fish
        ;;

    4)
        julia day04-julia/day04.jl
        ;;

    *)
        echo "Unknown or unavailable day"
        exit 1
esac
