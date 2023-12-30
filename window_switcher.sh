#!/usr/bin/env bash

## Author: Aditya Shakya (adi1090x)
## Github: @adi1090x
#
## Rofi   : Launcher (Modi Drun, Run, File Browser, Window)
#
## Available Styles
#
## type-1: style-1 to style-15
## type-2: style-1 to style-15
## type-3: style-1 to style-10
## type-4: style-1 to style-10
## type-5: style-1 to style-5
## type-6: style-1 to style-10
## type-7: style-1 to style-10

# Get a random number between 1 and 7 for the directory
random_dir=$((1 + RANDOM % 7))
dir="$HOME/.config/rofi/launchers/type-${random_dir}"

# Determine the maximum number of styles based on the type
max_styles=15
case $random_dir in
    3|4|6|7)
        max_styles=10
        ;;
    5)
        max_styles=5
        ;;
esac

# Get a random number between 1 and the determined maximum number of styles
random_style=$((1 + RANDOM % max_styles))
theme="style-${random_style}"

## Run
rofi \
    -modi window,drun \
    -show window \
    -theme "${dir}/${theme}.rasi"
