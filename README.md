# images

# build

- Install stack https://docs.haskellstack.org/en/stable/install_and_upgrade/
- run `stack setup`
- run `stack build`

# run

options:

- run `./run.sh`
- run `stack exec garden -- [input filename.gif] [number of frames] [output filename.gif]`
- see it fail because windows or something and do `stack exec garden -- -- [input filename.gif] [number of frames] [output filename.gif]` or something instead
- find the executable in `./stack-work/dist/[stuff]/build/garden/garden` and call it by hand
