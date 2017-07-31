#!/bin/sh

stack build
stack exec images-exe -- 16.gif 4 out.gif
