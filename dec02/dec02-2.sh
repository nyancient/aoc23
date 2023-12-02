#!/bin/bash
cube_powers() {
    declare -A cubes_seen=( ["red"]="0" ["green"]="0" ["blue"]="0" )
    for s in $@ ; do
        let number="$(echo $s | sed -e 's/[^0-9]*\([0-9]\+\).*/\1/')"
        color="$(echo $s | sed -e 's/[^a-z]*\([a-z]\+\).*/\1/')"
        let cubes_previously_seen=${cubes_seen[$color]}
        if (( $number > $cubes_previously_seen )) ; then
            cubes_seen[$color]=$number
        fi
    done
    let power=$((${cubes_seen['red']} * ${cubes_seen['blue']} * ${cubes_seen['green']}))
    echo $power
}

let total_power=0
while read ln ; do
    game_sets=$(echo $ln | cut -d ':' -f 2)
    let total_power=total_power+$(cube_powers $(echo $game_sets | tr ';, ' '  _'))
done
echo $total_power