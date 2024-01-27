# viper-data-collection
This is a backend to collect and benchmark user programs and metadata (with consent) from Viper verifiers and frontends for future evaluation. 
It consists of a PostgresQL database run in a Docker container to store the data and a webserver to submit programs and post queries about stored data.
## Prerequisites:
- JDK 11
- [SBT](https://www.scala-sbt.org)
- [Z3Prover](https://github.com/Z3Prover/z3)
  * Make sure to `export Z3_EXE="path/to/z3"` in your shell file
- [Boogie](https://github.com/boogie-org/boogie)
  * Make sure to `export BOOGIE_EXE="path/to/boogie"` in your shell file
- [Docker](https://www.docker.com)
- [Docker-Compose](https://docs.docker.com/compose/)

## Setup

#### On Server

- Clone this repository recursively: `git clone --recursive https://github.com/viperproject/viper-data-collection`
- Make sure all files in `bash_scripts` are marked executable.
- Run `sbt compile`
- Set up port forwarding from `localhost:WEBSERVER_LOCAL_PORT` (can be found in `util/Config`) to your desired outbound port to be able to access the API.

#### Code to modify

For programs to be submitted to your instance, change the following files:

- Silver: In `viper.silver.utility.ProgramSubmitter` change `val API_HOST` in the trait `ProgramSubmitter` to `http://server_ip/outbound_port`. This implementation is used by Silicon, Carbon, ViperServer, Gobra and Nagini.

- Prusti: In `prusti_utils::program_submitter` change `const API_HOST` to `http://server_ip/outbound_port`.

- vdc-query-frontend: In `queryFrontend.Config` change `val API_HOST` to `http://server_ip/outbound_port`.

## Usage

#### Running Backend
Running `./run.sh` will start the Docker database container and the API webserver.

#### Submitting Programs

User programs will only be submitted with their consent. To submit their programs, users have to explicitly pass a flag in the respective frontend:
- Silicon, Carbon, Gobra: Add `--submitForEvaluation` to the command.
- ViperServer: Pass `--submitForEvaluation` as a verifier option.
- Prusti: There are different ways to pass options to Prusti, I would recommend adding the line `submit_for_evaluation = true` to your `Prusti.toml`
- Nagini: Add `--submit-for-evaluation` to the command.
#### Querying WebAPI
To access the API, either query `http://server_ip/outbound_port/query_endpoint` directly, or use the pre-written queries in `vdc-query-frontend/src/main/scala/queryFrontend/APIQueries`, which handle correct serialization of the query data.

#### Database Backups
To back up the database, run `bash_scripts/db_backup.sh`. This will store a compressed backup in `db/backups/`. To restore an older version, run `bash_scripts/db_restore.sh backup_filename`. The file has to be found in `db/backups/`.
