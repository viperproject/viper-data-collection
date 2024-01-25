# Setup

- Clone this repository recursively: `git clone --recursive https://github.com/Simon-Hostettler/viper-data-collection`
- Make sure all files in `bash_scripts` are marked executable.
- Run `sbt compile`

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

Set up port forwarding from `localhost:WEBSERVER_LOCAL_PORT` (can be found in `util/Config`) to your desired outbound port to be able to access the API.

To submit programs to this instance, change the following files:

- Silver: In `viper.silver.utility.ProgramSubmitter` change `val API_HOST` in the trait `ProgramSubmitter` to `http://server_ip/outbound_port`. This implementation is used by Silicon, Carbon, ViperServer, Gobra and Nagini.

- Prusti: In `prusti_utils::program_submitter` change `const API_HOST` to `http://server_ip/outbound_port`.

- vdc-query-frontend: In `queryFrontend.Config` change `val API_HOST` to `http://server_ip/outbound_port`.



### Usage

Running `./run.sh` will start the Docker database container and the API webserver.

To access the API, query `http://server_ip/outbound_port/query_endpoint`. For relevant endpoints see `vdc-query-frontend/src/main/scala/queryFrontend/APIQueries`.

To back up the database, run `bash_scripts/db_backup.sh`. This will store a compressed backup in `db/backups/`. To restore an older version, run `bash_scripts/db_restore.sh backup_filename`. The file has to be found in `db/backups/`.
