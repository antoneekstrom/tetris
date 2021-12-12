# Setting up the development environment
In order to run the program one has to install a couple of dependencies in the development container.

## Start the dev container
Load the project in the dev container which is specified in the `.devcontainer` folder.
This can be done using the VSCode extension "Remote - Containers".

## Install dependencies
Run the following commands on the command line in the dev container:

```bash
> ghcup install stack
> sudo apt-get update
> sudo apt-get install freeglut3-dev
> cabal install gloss
```