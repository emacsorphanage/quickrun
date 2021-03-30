#!/bin/bash

function endlessLoop () {
    local step=1
    while true
    do
        printf "Endless loop step %d\n" $step
        ((step+=1))
        sleep 1
    done
}

endlessLoop
