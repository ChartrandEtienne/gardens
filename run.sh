#!/bin/sh

stack build
stack exec images-exe
feh after.png
