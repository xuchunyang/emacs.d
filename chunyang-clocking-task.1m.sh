#!/bin/bash

# A BitBar <https://github.com/matryer/bitbar> plugin to display clocking task
# on OS X menu bar.

# Using sed to produce string like this "[... ...]"
emacsclient --eval '(chunyang-clocking-task)' | sed 's/^"\[\(.*\)\]"$/\1/'
