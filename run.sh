#!/bin/sh

stack build
stack exec garden -- 16.gif 4 out.gif
