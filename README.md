# Setup

### Prerequisites:
- JDK 11
- [SBT](https://www.scala-sbt.org)
- [Z3Prover](https://github.com/Z3Prover/z3)
    * Make sure to `export Z3_EXE="path/to/z3"` in your shell file
- [Boogie](https://github.com/boogie-org/boogie)
    * Make sure to `export BOOGIE_EXE="path/to/boogie"` in your shell file
- [Docker](https://www.docker.com)
- [Docker-Compose](https://docs.docker.com/compose/)

### Configuration

Set up port forwarding from `localhost:8080` to your desired outbound port to be able to access the API.

### Usage

Running `./run.sh` will start the Docker database container and the API webserver.