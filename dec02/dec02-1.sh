#!/bin/bash
declare -A color_limits=( ["red"]="12" ["green"]="13" ["blue"]="14" )

set_possible() {
    for g in $@ ; do
        let number="$(echo $g | sed -e 's/[^0-9]*\([0-9]\+\).*/\1/')"
        color="$(echo $g | sed -e 's/[^a-z]*\([a-z]\+\).*/\1/')"
        let color_limit=${color_limits[$color]}
        if (( $number > $color_limit )) ; then
            return 1
        fi
    done
    return 0
}

possible() {
    for s in $@ ; do
        set_possible $(echo $s | tr ',' '\n') || return 1
    done
    return 0
}

let possible_game_id_sums=0
while read ln ; do
    let game_id=$(echo $ln | awk '{print $2}' | sed -e 's/://g')
    game_sets=$(echo $ln | cut -d ':' -f 2)
    if possible $(echo $game_sets | tr '; ' ' _') ; then
        let possible_game_id_sums=$possible_game_id_sums+$game_id
    fi
done
echo $possible_game_id_sums