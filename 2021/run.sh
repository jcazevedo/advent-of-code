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

    5)
        node day05-javascript/day05.js
        ;;

    6)
        sbcl --script day06-common-lisp/day06.lisp
        ;;

    7)
        maxima --very-quiet -b day07-maxima/day07.mac
        ;;

    8)
        clang day08-objective-c/day08.m -o day08 && ./day08
        ;;

    9)
        fpc day09-pascal/day09.pas && ./day09-pascal/day09
        ;;

    10)
        # This is currently assuming PostgreSQL is running.
        # docker run -p 5432:5432 -e POSTGRES_PASSWORD=postgres -v <inputs>:/inputs postgres:12.5-alpine
        psql "postgresql://postgres:postgres@localhost:5432/postgres" -f day10-plpgsql/day10.sql
        ;;

    *)
        echo "Unknown or unavailable day"
        exit 1
esac
